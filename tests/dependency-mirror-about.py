"""Exercise the packaged cgit README filter, including hostile upstream input."""

import pathlib
import subprocess
import sys
import tempfile
import unittest

FILTER = sys.argv.pop(1)


class ReadmeFilterTests(unittest.TestCase):
    def render(self, filename, text):
        return subprocess.run(
            [FILTER, filename], input=text, text=True, capture_output=True,
            check=True, timeout=30,
        ).stdout

    def test_markdown(self):
        output = self.render("README.md", "# Heading\n\n**bold**\n\n```sh\necho hello\n```\n")
        self.assertIn("<h1>Heading</h1>", output)
        self.assertIn("<strong>bold</strong>", output)
        self.assertIn("echo hello", output)

    def test_asciidoc(self):
        output = self.render("README.adoc", "= Project\n\n== Overview\n\n*bold*\n")
        self.assertIn("Overview</h2>", output)
        self.assertIn("<strong>bold</strong>", output)

    def test_plaintext(self):
        self.assertIn("&lt;script&gt;", self.render("README", "<script>bad()</script>"))

    def test_markdown_sanitized(self):
        output = self.render("README.md", '<script>bad()</script><img src="x" onerror="bad()"><a href="javascript:bad()">link</a><form action="/edit/delete"><input name="id"></form>')
        for unsafe in ["<script", "onerror", "javascript:", "<form", "<input"]:
            self.assertNotIn(unsafe, output)

    def test_asciidoc_sanitized(self):
        output = self.render("README.adoc", '++++\n<script>bad()</script><img src="x" onerror="bad()">\n++++\n')
        self.assertNotIn("<script", output)
        self.assertNotIn("onerror", output)

    def test_asciidoc_cannot_read_local_file(self):
        with tempfile.TemporaryDirectory() as directory:
            source = pathlib.Path(directory) / "not-for-readme.txt"
            source.write_text("LOCAL_FILE_CONTENT_MUST_NOT_LEAK")
            output = self.render("README.adoc", f"include::{source}[]\n")
            self.assertNotIn("LOCAL_FILE_CONTENT_MUST_NOT_LEAK", output)

    def test_size_limit(self):
        self.assertIn("too large", self.render("README.md", "x" * 1_000_001))


if __name__ == "__main__":
    unittest.main()
