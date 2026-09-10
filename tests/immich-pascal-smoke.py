"""Run on the target GPU with the Pascal validation Python, never during a sandbox build."""
import ctypes
import json
import tempfile
from pathlib import Path

import numpy as np
import onnx
import onnxruntime as ort
from onnx import TensorProto, helper, numpy_helper

rng = np.random.default_rng(42)
with tempfile.TemporaryDirectory(prefix="immich-pascal-") as tmp:
    for operation, shape, weights in [
        ("MatMul", (32, 64), (64, 16)),
        ("Conv", (1, 3, 32, 32), (8, 3, 3, 3)),
    ]:
        x = rng.normal(size=shape).astype(np.float32)
        w = rng.normal(size=weights).astype(np.float32)
        output_shape = (32, 16) if operation == "MatMul" else (1, 8, 30, 30)
        graph = helper.make_graph(
            [helper.make_node(operation, ["x", "w"], ["intermediate"]),
             helper.make_node("Relu", ["intermediate"], ["y"])],
            operation,
            [helper.make_tensor_value_info("x", TensorProto.FLOAT, shape)],
            [helper.make_tensor_value_info("y", TensorProto.FLOAT, output_shape)],
            [numpy_helper.from_array(w, "w")],
        )
        model = helper.make_model(graph, opset_imports=[helper.make_opsetid("", 18)])
        model.ir_version = 10
        onnx.checker.check_model(model)
        options = ort.SessionOptions()
        options.enable_profiling = True
        options.profile_file_prefix = str(Path(tmp) / operation)
        gpu = ort.InferenceSession(model.SerializeToString(), options,
                                   providers=["CUDAExecutionProvider", "CPUExecutionProvider"])
        gpu.disable_fallback()
        cpu = ort.InferenceSession(model.SerializeToString(), providers=["CPUExecutionProvider"])
        actual = gpu.run(None, {"x": x})[0]
        expected = cpu.run(None, {"x": x})[0]
        assert np.isfinite(actual).all()
        np.testing.assert_allclose(actual, expected, rtol=1e-4, atol=1e-4)
        events = json.loads(Path(gpu.end_profiling()).read_text())
        cuda_ops = [event["args"].get("op_name") for event in events
                    if event.get("args", {}).get("provider") == "CUDAExecutionProvider"]
        assert any(operation.lower() in op.lower() for op in cuda_ops), cuda_ops
        print(json.dumps({"operation": operation, "cuda_ops": cuda_ops,
                          "max_absolute_error": float(np.max(np.abs(actual - expected)))}), flush=True)

cudnn = ctypes.CDLL("libcudnn.so.9")
cudnn.cudnnGetVersion.restype = ctypes.c_size_t
assert cudnn.cudnnGetVersion() == 91002
print(json.dumps({"onnxruntime": ort.__version__, "cudnn": cudnn.cudnnGetVersion(),
                  "status": "passed"}), flush=True)
