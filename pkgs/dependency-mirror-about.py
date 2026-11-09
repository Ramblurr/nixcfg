"""Render repository READMEs for cgit without trusting upstream HTML."""

import html
import pathlib
import subprocess
import sys

import bleach
import markdown


def render(filename, text):
    suffix = pathlib.Path(filename).suffix.lower()
    if suffix in {".adoc", ".asciidoc", ".asc"}:
        rendered = subprocess.run(
            ["asciidoctor", "--safe-mode", "secure", "--no-header-footer", "-o", "-", "-"],
            input=text, text=True, capture_output=True, check=True, timeout=15,
        ).stdout
    elif suffix in {".md", ".markdown", ".mkd", ".mdown"}:
        rendered = markdown.markdown(text, extensions=["fenced_code", "tables"])
    else:
        rendered = "<pre>" + html.escape(text) + "</pre>"
    # Both Markdown and AsciiDoc can contain raw HTML. The editor shares this origin.
    return bleach.clean(
        rendered,
        tags={"a", "p", "div", "span", "h1", "h2", "h3", "h4", "h5", "h6",
              "pre", "code", "blockquote", "ul", "ol", "li", "dl", "dt", "dd",
              "table", "thead", "tbody", "tr", "th", "td", "br", "hr", "strong",
              "em", "b", "i", "del", "sup", "sub", "img", "details", "summary"},
        attributes={"*": ["class", "id"], "a": ["href", "title"],
                    "img": ["src", "alt", "title", "width", "height"],
                    "th": ["colspan", "rowspan"], "td": ["colspan", "rowspan"]},
        protocols={"http", "https", "mailto"}, strip=True,
    )


if __name__ == "__main__":
    text = sys.stdin.read(1_000_001)
    if len(text) > 1_000_000:
        print("<p>README is too large to render. Use the repository tree to read it.</p>")
    else:
        try:
            print(render(sys.argv[1], text))
        except (subprocess.SubprocessError, OSError):
            print("<pre>" + html.escape(text) + "</pre>")
