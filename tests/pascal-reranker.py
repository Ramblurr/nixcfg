"""Hardware test for the maintained Sentence Transformers eager-FP32 alternative.

Run with the separate pascal-reranker-python environment, a writable HF_HOME,
LD_LIBRARY_PATH=/run/opengl-driver/lib, and a writable current directory.
The temporary HTTP server listens only on an ephemeral loopback port.
"""
import hashlib
import json
import socket
import threading
import time
from pathlib import Path

import numpy as np
import requests
import torch
import uvicorn
from fastapi import FastAPI
from huggingface_hub import snapshot_download
from pydantic import BaseModel, Field
from sentence_transformers import CrossEncoder

model_name = "cross-encoder/ms-marco-MiniLM-L6-v2"
assert torch.cuda.is_available()
assert torch.cuda.get_device_capability() == (6, 1)
assert "sm_61" in torch.cuda.get_arch_list() or "sm_60" in torch.cuda.get_arch_list()
model = CrossEncoder(model_name, device="cpu", trust_remote_code=False,
                     model_kwargs={"dtype": torch.float32, "attn_implementation": "eager"})
query = "What is the capital of France?"
documents = ["Paris is the capital of France.", "Berlin is the capital of Germany.",
             "Bananas grow in tropical climates."]
pairs = [(query, document) for document in documents]
cpu = model.predict(pairs, show_progress_bar=False)
model.to("cuda")
assert next(model.parameters()).device.type == "cuda"
assert next(model.parameters()).dtype == torch.float32
model.predict(pairs, show_progress_bar=False)  # warm up before profiling
with torch.profiler.profile(activities=[torch.profiler.ProfilerActivity.CPU,
                                       torch.profiler.ProfilerActivity.CUDA]) as profile:
    gpu = model.predict(pairs, show_progress_bar=False)
    torch.cuda.synchronize()
profile.export_chrome_trace("reranker-cuda-profile.json")
cuda_events = [event for event in profile.events() if str(event.device_type) == "DeviceType.CUDA"]
assert cuda_events, "No CUDA events recorded"
arithmetic_kernels = {event.name for event in cuda_events
                      if any(name in event.name.lower() for name in ["gemm", "gemv", "softmax", "elementwise"])}
assert arithmetic_kernels, "CUDA trace contains no recognized arithmetic kernels"
np.testing.assert_allclose(gpu, cpu, rtol=1e-4, atol=1e-4)
assert int(np.argmax(gpu)) == 0
snapshot = Path(snapshot_download(model_name, local_files_only=True))
weights = {}
for path in [*snapshot.glob("*.safetensors"), *snapshot.glob("pytorch_model*.bin")]:
    with path.open("rb") as handle:
        weights[path.name] = hashlib.file_digest(handle, "sha256").hexdigest()
assert weights
print(json.dumps({"model_revision": snapshot.name, "weights_sha256": weights}), flush=True)
print(json.dumps({"model": model_name, "torch": torch.__version__, "compiled_cuda": torch.version.cuda,
                  "cudnn": torch.backends.cudnn.version(), "architectures": torch.cuda.get_arch_list(),
                  "cpu_scores": cpu.tolist(), "gpu_scores": gpu.tolist(), "cuda_events": len(cuda_events),
                  "distinct_cuda_arithmetic_kernels": len(arithmetic_kernels),
                  "max_absolute_error": float(np.max(np.abs(cpu - gpu))), "status": "passed"}), flush=True)

# Minimal adapter only; the maintained inference implementation is CrossEncoder.
app = FastAPI()


class RerankRequest(BaseModel):
    query: str = Field(min_length=1, max_length=4096)
    texts: list[str] = Field(min_length=1, max_length=32)


@app.post("/rerank")
async def rerank(request: RerankRequest):
    scores = model.predict([(request.query, text) for text in request.texts], show_progress_bar=False)
    return sorted([{"index": index, "score": float(score)} for index, score in enumerate(scores)],
                  key=lambda item: item["score"], reverse=True)


listener = socket.socket()
listener.bind(("127.0.0.1", 0))
port = listener.getsockname()[1]
server = uvicorn.Server(uvicorn.Config(app, log_level="warning"))
thread = threading.Thread(target=server.run, kwargs={"sockets": [listener]}, daemon=True)
thread.start()
try:
    deadline = time.monotonic() + 10
    while not server.started and time.monotonic() < deadline:
        time.sleep(0.1)
    assert server.started
    response = requests.post(f"http://127.0.0.1:{port}/rerank",
                             json={"query": query, "texts": documents}, timeout=30)
    response.raise_for_status()
    ranked = response.json()
    assert ranked[0]["index"] == 0
    np.testing.assert_allclose([row["score"] for row in sorted(ranked, key=lambda row: row["index"])], gpu)
    print(json.dumps({"http_rerank": ranked, "status": "passed"}), flush=True)
finally:
    server.should_exit = True
    thread.join(timeout=10)
    listener.close()
