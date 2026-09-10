"""Exercise real Immich models, comparing ORT CUDA outputs to CPU and profiling placement.

Run using the installed ML service's Python/PYTHONPATH, with a disposable cache,
working directory, and a public face photograph supplied as argv[1]. Downloads
and profiles stay in that directory. This is an on-hardware validation, not a mock test.
"""
import gc
import hashlib
import itertools
import json
import sys
from pathlib import Path

import numpy as np
import onnxruntime as ort
from PIL import Image, ImageDraw, ImageFont

OriginalSession = ort.InferenceSession
sessions = []
profile_ids = itertools.count()


class ComparedSession(OriginalSession):
    def __init__(self, path, sess_options=None, providers=None, provider_options=None, **kwargs):
        opts = sess_options or ort.SessionOptions()
        opts.enable_profiling = True
        opts.profile_file_prefix = str(Path.cwd() / f"profile-{next(profile_ids)}")
        super().__init__(path, opts, providers=providers, provider_options=provider_options, **kwargs)
        self.disable_fallback()
        assert "CUDAExecutionProvider" in self.get_providers(), self.get_providers()
        self.model_path = str(path)
        self.comparisons = []
        sessions.append(self)

    def run(self, output_names, input_feed, run_options=None):
        actual = super().run(output_names, input_feed, run_options)
        opts = ort.SessionOptions()
        opts.intra_op_num_threads = 2
        cpu = OriginalSession(self.model_path, opts, providers=["CPUExecutionProvider"])
        expected = cpu.run(output_names, input_feed, run_options)
        for a, b in zip(actual, expected, strict=True):
            assert np.isfinite(a).all()
            np.testing.assert_allclose(a, b, rtol=1e-3, atol=1e-3)
        self.comparisons.append(max(float(np.max(np.abs(a - b))) for a, b in zip(actual, expected)))
        return actual


ort.InferenceSession = ComparedSession
from immich_ml.models.clip.textual import OpenClipTextualEncoder
from immich_ml.models.clip.visual import OpenClipVisualEncoder
from immich_ml.models.facial_recognition.detection import FaceDetector
from immich_ml.models.facial_recognition.recognition import FaceRecognizer
from immich_ml.models.ocr.detection import TextDetector
from immich_ml.models.ocr.recognition import TextRecognizer


def finish(label):
    reports = []
    for session in sessions:
        events = json.loads(Path(session.end_profiling()).read_text())
        cuda_ops = [e["args"].get("op_name", "") for e in events
                    if e.get("args", {}).get("provider") == "CUDAExecutionProvider"]
        # Loading is insufficient: require substantial arithmetic in a real run.
        assert session.comparisons and any(
            any(k in op.lower() for k in ["conv", "matmul", "gemm", "attention"]) for op in cuda_ops
        ), (label, cuda_ops)
        path = Path(session.model_path)
        reports.append({"model": session.model_path, "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
                        "cuda_nodes": len(cuda_ops), "cuda_ops": sorted(set(cuda_ops)),
                        "max_absolute_errors": session.comparisons})
    print(json.dumps({"path": label, "profiles": reports, "status": "passed"}), flush=True)
    sessions.clear()
    gc.collect()


image = Image.open(sys.argv[1]).convert("RGB")
text_model = OpenClipTextualEncoder("ViT-B-32__openai")
text_embeddings = [text_model.predict(text) for text in ["a photograph of a person", "a red sports car"]]
assert text_embeddings[0] != text_embeddings[1]
assert len(json.loads(text_embeddings[0])) == 512
finish("smart-search-text")
del text_model

visual = OpenClipVisualEncoder("ViT-B-32__openai")
embedding = visual.predict(image)
assert len(json.loads(embedding)) == 512
text_vectors = np.array([json.loads(value) for value in text_embeddings])
image_vector = np.array(json.loads(embedding))
similarities = (text_vectors @ image_vector) / (
    np.linalg.norm(text_vectors, axis=1) * np.linalg.norm(image_vector))
assert similarities[0] > similarities[1], similarities
print(json.dumps({"person_vs_car_similarity": similarities.tolist()}), flush=True)
finish("smart-search-image")
del visual

detector = FaceDetector("buffalo_l")
faces = detector.predict(Path(sys.argv[1]).read_bytes())
assert len(faces["boxes"]) > 0, "No real face detected"
print(json.dumps({"faces": len(faces["boxes"]), "scores": faces["scores"].tolist()}), flush=True)
finish("face-detection")
del detector

recognizer = FaceRecognizer("buffalo_l")
recognizer.batch_size = 1
recognized = recognizer.predict(image, faces)
assert len(recognized) == len(faces["boxes"])
assert all(len(json.loads(face["embedding"])) == 512 for face in recognized)
finish("face-recognition")
del recognizer

text_image = Image.new("RGB", (1000, 240), "white")
draw = ImageDraw.Draw(text_image)
font = ImageFont.load_default(size=60)
draw.text((40, 70), "PASCAL GPU TEST 1070", fill="black", font=font)
text_image.save("ocr-fixture.png")
ocr_detector = TextDetector("PP-OCRv5_mobile")
boxes = ocr_detector.predict(text_image)
assert len(boxes["boxes"]) > 0, "No text detected"
finish("ocr-detection")
del ocr_detector

ocr_recognizer = TextRecognizer("PP-OCRv5_mobile", min_score=0.5)
result = ocr_recognizer.predict(text_image, boxes)
assert "PASCAL" in " ".join(result["text"]).upper(), result["text"]
print(json.dumps({"ocr_text": result["text"]}), flush=True)
finish("ocr-recognition")
