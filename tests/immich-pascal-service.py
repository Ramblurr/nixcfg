"""Validate the running loopback ML service and concurrent Jellyfin FFmpeg NVENC.

Run as immich in a writable validation directory. Arguments: public portrait,
OCR fixture, and the Jellyfin FFmpeg bin directory used by this deployment.
"""
import atexit
import json
import subprocess
import sys
import time
from pathlib import Path

import requests

portrait, ocr_image, ffmpeg_bin = map(Path, sys.argv[1:])
url = "http://127.0.0.1:3003"
assert requests.get(f"{url}/ping", timeout=10).text == "pong"


def predict(entries, image=None, text=None):
    files = {"image": ("fixture.png", image.read_bytes())} if image else None
    response = requests.post(f"{url}/predict", data={"entries": json.dumps(entries), **({"text": text} if text else {})},
                             files=files, timeout=180)
    response.raise_for_status()
    return response.json()


clip = "ViT-B-32__openai"
result = predict({"clip": {"textual": {"modelName": clip}}}, text="a photograph of a person")
assert len(json.loads(result["clip"])) == 512
result = predict({"clip": {"visual": {"modelName": clip}},
                  "facial-recognition": {"detection": {"modelName": "buffalo_l"},
                                         "recognition": {"modelName": "buffalo_l"}}}, image=portrait)
assert len(json.loads(result["clip"])) == 512
assert len(result["facial-recognition"]) >= 1
result = predict({"ocr": {"detection": {"modelName": "PP-OCRv5_mobile"},
                          "recognition": {"modelName": "PP-OCRv5_mobile"}}}, image=ocr_image)
assert "PASCAL" in " ".join(result["ocr"]["text"]).upper(), result
print(json.dumps({"http_paths": ["clip-text", "clip-image", "faces", "ocr"], "status": "passed"}), flush=True)

processes = []


@atexit.register
def stop_encoders():
    for _, _, log, process in processes:
        if process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()
        log.close()


for codec in ["h264", "hevc"]:
    output = Path(f"{codec}-nvenc.mp4")
    log = Path(f"{codec}-nvenc.log").open("w")
    command = [str(ffmpeg_bin / "ffmpeg"), "-hide_banner", "-y", "-re", "-f", "lavfi",
               "-i", "testsrc2=size=1280x720:rate=30", "-t", "10", "-an",
               "-c:v", f"{codec}_nvenc", "-pix_fmt", "yuv420p", str(output)]
    processes.append((codec, output, log, subprocess.Popen(command, stdout=log, stderr=log)))

requests_completed = 0
peak_memory_mib = 0
peak_utilization = 0
while any(process.poll() is None for _, _, _, process in processes):
    predict({"clip": {"visual": {"modelName": clip}}}, image=portrait)
    sample = subprocess.check_output(["nvidia-smi", "--query-gpu=memory.used,utilization.gpu",
                                      "--format=csv,noheader,nounits"], text=True)
    memory, utilization = map(int, sample.strip().split(","))
    peak_memory_mib = max(peak_memory_mib, memory)
    peak_utilization = max(peak_utilization, utilization)
    requests_completed += 1
    time.sleep(0.2)

for codec, output, log, process in processes:
    assert process.wait() == 0, f"{codec} NVENC failed; see its log"
    log.close()
    info = json.loads(subprocess.check_output([
        str(ffmpeg_bin / "ffprobe"), "-v", "error", "-count_frames", "-show_entries",
        "stream=codec_name,nb_read_frames,width,height", "-of", "json", str(output)], text=True))
    stream = info["streams"][0]
    assert stream["codec_name"] == codec and int(stream["nb_read_frames"]) == 300, info
    print(json.dumps({"nvenc": codec, "stream": stream, "status": "passed"}), flush=True)

assert requests_completed > 0
print(json.dumps({"concurrent_ml_requests": requests_completed, "sampled_peak_memory_mib": peak_memory_mib,
                  "sampled_peak_gpu_utilization": peak_utilization, "status": "passed"}), flush=True)
