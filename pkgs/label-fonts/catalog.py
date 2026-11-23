"""Build the SVG label font catalog.

  catalog.py select SRC
      Print the google/fonts family directories to install, relative to SRC.
  catalog.py google SRC FONTS_DIR OUT
      Write the Google catalog for the families installed in FONTS_DIR/<dir>.
  catalog.py merge GOOGLE_JSON OUT SVG_FONT_DIR...
      Add the bundled families of every SVG font dir after the first, then
      check that resvg finds every catalog family in exactly those dirs.

resvg --list-fonts is the reader for family, style and weight, because the
family name it reports is the one an SVG font-family must use.
"""

import codecs
import glob
import html.parser
import json
import os
import re
import subprocess
import sys

LICENSE_DIRS = ["apache", "ofl", "ufl"]
# The directories nixpkgs' google-fonts postPatch removes, plus Adobe Blank.
EXCLUDED = {"ofl/cabincondensed", "ofl/signikanegative", "ofl/signikanegativesc", "ofl/adobeblank"}
STYLES = ["normal", "italic", "oblique"]
LIST_LINE = re.compile(r"^(?P<path>.+?): '(?P<families>.*)', \d+, (?P<style>\w+), (?P<weight>\d+), \w+$")


def parse_textproto(text):
    """Parse protobuf text format into {field: [values]}; messages are dicts."""
    tokens = re.findall(r'"(?:[^"\\]|\\.)*"|#.*|[{}:]|[^\s{}:"#]+', text)
    tokens = [t for t in tokens if not t.startswith("#")]
    pos = 0

    def message():
        nonlocal pos
        fields = {}
        while pos < len(tokens) and tokens[pos] != "}":
            name = tokens[pos]
            pos += 1
            if tokens[pos] == ":":
                pos += 1
            if tokens[pos] == "{":
                pos += 1
                value = message()
                pos += 1
            else:
                value = scalar(tokens[pos])
                pos += 1
                while pos < len(tokens) and tokens[pos].startswith('"') and isinstance(value, str):
                    value += scalar(tokens[pos])
                    pos += 1
            fields.setdefault(name, []).append(value)
        return fields

    def scalar(token):
        if token.startswith('"'):
            return codecs.escape_decode(token[1:-1].encode("utf-8"))[0].decode("utf-8")
        try:
            return float(token) if "." in token else int(token)
        except ValueError:
            return token

    return message()


def metadata(family_dir):
    with open(os.path.join(family_dir, "METADATA.pb"), encoding="utf-8") as f:
        return parse_textproto(f.read())


class TextExtractor(html.parser.HTMLParser):
    def __init__(self):
        super().__init__()
        self.parts = []

    def handle_data(self, data):
        self.parts.append(data)

    def handle_starttag(self, tag, attrs):
        self.parts.append(" ")

    def handle_endtag(self, tag):
        self.parts.append(" ")


def description(family_dir):
    """Plain text of DESCRIPTION.en_us.html, or of the article newer families use instead."""
    for name in ["DESCRIPTION.en_us.html", "article/ARTICLE.en_us.html"]:
        path = os.path.join(family_dir, name)
        if os.path.exists(path):
            extractor = TextExtractor()
            with open(path, encoding="utf-8") as f:
                extractor.feed(f.read())
            text = " ".join("".join(extractor.parts).split())
            if text:
                return text
    return ""


def select(src):
    for license_dir in LICENSE_DIRS:
        for meta_path in sorted(glob.glob(os.path.join(src, license_dir, "*", "METADATA.pb"))):
            family_dir = os.path.dirname(meta_path)
            rel = os.path.relpath(family_dir, src)
            if rel in EXCLUDED or rel.endswith("_todelist"):
                continue
            meta = metadata(family_dir)
            if "primary_script" not in meta and "latin" in meta.get("subsets", []):
                yield rel


def list_fonts(font_dirs):
    """Map each font file resvg loads from font_dirs to (family, style, weight)."""
    args = ["resvg", "--skip-system-fonts", "--list-fonts"]
    for font_dir in font_dirs:
        args += ["--use-fonts-dir", font_dir]
    out = subprocess.run(args, check=True, capture_output=True, text=True)
    faces = {}
    for line in (out.stdout + out.stderr).splitlines():
        match = LIST_LINE.match(line)
        if match:
            # The first name is the English (US) one when the font has it.
            family = match["families"].split("', '")[0].rsplit(" (", 1)[0]
            faces[match["path"]] = (family, match["style"].lower(), int(match["weight"]))
    return faces


def group_faces(faces, key):
    """Group faces by (key(path), family) into sorted style and weight lists."""
    groups = {}
    for path, (family, style, weight) in faces.items():
        styles, weights = groups.setdefault((key(path), family), (set(), set()))
        styles.add(style)
        weights.add(weight)
    return {k: (sorted(s, key=STYLES.index), sorted(w)) for k, (s, w) in groups.items()}


def number(value):
    return int(value) if value == int(value) else value


def entry(family, source, styles, weights, meta=None, desc=""):
    meta = meta or {}
    return {
        "family": family,
        "source": source,
        "category": meta.get("category", []),
        "designer": meta.get("designer", [""])[0],
        "license": meta.get("license", [""])[0],
        "subsets": [s for s in meta.get("subsets", []) if s != "menu"],
        "styles": styles,
        "weights": weights,
        "axes": [{"tag": a["tag"][0], "min": number(a["min_value"][0]), "max": number(a["max_value"][0])} for a in meta.get("axes", [])],
        "description": desc,
    }


def unique_by_family(entries):
    families = {}
    for e in entries:
        families.setdefault(e["family"], []).append(e)
    duplicates = sorted(f for f, es in families.items() if len(es) > 1)
    if duplicates:
        sys.exit(f"families reported by more than one directory: {duplicates}")
    return sorted(entries, key=lambda e: e["family"])


def write(path, entries):
    with open(path, "w", encoding="utf-8") as f:
        json.dump({"families": entries}, f, ensure_ascii=False, indent=1)
        f.write("\n")


def google(src, fonts_dir, out):
    faces = list_fonts([fonts_dir])
    unloaded = sorted(set(glob.glob(os.path.join(fonts_dir, "*", "*", "*.ttf"))) - faces.keys())
    if unloaded:
        sys.exit(f"resvg could not load: {unloaded}")
    groups = group_faces(faces, lambda path: os.path.relpath(os.path.dirname(path), fonts_dir))
    entries = [
        entry(family, "google-fonts", styles, weights, metadata(os.path.join(src, rel)), description(os.path.join(src, rel)))
        for (rel, family), (styles, weights) in groups.items()
    ]
    write(out, unique_by_family(entries))


def merge(google_json, out, svg_font_dirs):
    with open(google_json, encoding="utf-8") as f:
        entries = json.load(f)["families"]
    for (_, family), (styles, weights) in group_faces(list_fonts(svg_font_dirs[1:]), lambda path: None).items():
        entries.append(entry(family, "bundled", styles, weights))
    entries = unique_by_family(entries)
    found = {family for family, _, _ in list_fonts(svg_font_dirs).values()}
    absent = sorted(e["family"] for e in entries if e["family"] not in found)
    if absent:
        sys.exit(f"resvg does not find these catalog families in {svg_font_dirs}: {absent}")
    write(out, entries)


if __name__ == "__main__":
    command, *args = sys.argv[1:]
    if command == "select":
        print("\n".join(select(*args)))
    elif command == "google":
        google(*args)
    elif command == "merge":
        merge(args[0], args[1], args[2:])
    else:
        sys.exit(__doc__)
