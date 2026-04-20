---
name: 3d-print-organize
description: Organize the flat 3D-print model library at `~/src/3dprinting` into one human-readable directory per model, using memory-word-first folder names, keeping upstream zip archives with their extracted contents, and preserving custom PrusaSlicer `.3mf` and OpenSCAD files without renaming internal files. Use when Codex needs to import, name, clean up, or reorganize 3D-print model folders in this library, including when the human provides a Printables URL or model id and wants the model imported into the library.
---

# 3D Print Organization

Use the library root `~/src/3dprinting`.

Keep the library flat. Create one top-level directory per model or model family.

Do not invent a nested taxonomy unless the existing library already has a clear exception that requires it.

## Support Files

Use the sibling files in this skill directory when working with Printables:

- `printables-api.md` records only the confirmed GraphQL operations and schema drift notes.
- `scratch_printables.clj` is the ad hoc Babashka/Clojure scratchpad for live API work.

Keep `printables-api.md` current as new operations are verified. Keep `scratch_printables.clj` focused on compact, working examples.

## REPL Workflow

Use a Babashka nREPL for ad hoc Printables API work.

- If you already know the port, verify the REPL with:
  `BREPL_PORT=<port> brepl '(+ 1 2)'`
- If that returns `3`, reuse the existing REPL.
- If there is no running REPL, start one with:
  `bb --nrepl-server 0`
- Babashka prints the chosen port on startup. Use that port with `brepl`.
- Load the scratch file with:
  `BREPL_PORT=<port> brepl -f .agents/skills/3d-print-organize/scratch_printables.clj`

## Directory Naming

Name the model directory in human-readable words, not slug form.

- Prefer names like `P.I.N.D.A Adjusting Tool`, not `pinda-adjusting-tool`.
- Put the memory word first: choose the word the human will mentally scan for months later.
- Remove filler words that do not help retrieval.
- Keep the name concise, specific, and readable.
- Preserve meaningful words such as product names, brands, variants, or part purpose when they help retrieval.

Example:

- Source model title: `Simple Super P.I.N.D.A Adjusting Tool`
- Directory name: `P.I.N.D.A Adjusting Tool`

## Directory Contents

Treat the model directory itself as the working folder.

- Put the original `.zip` files from Printables in that directory.
- Extract the archive into that same directory.
- Keep all files from the zip.
- Do not rename extracted files.
- Keep custom PrusaSlicer `.3mf` files in that same directory.
- Keep custom OpenSCAD files in that same directory.

Over time a directory may contain upstream zip files, extracted upstream files, and custom local files together. That is normal.

## Workflow

1. Inspect the incoming archive or folder.
2. Choose the directory name using the memory-word-first rule.
3. Create the model directory directly under `~/src/3dprinting`.
4. Copy or move the upstream `.zip` into that directory.
5. Extract the archive into that directory.
6. Leave filenames unchanged.
7. Add custom `.3mf` or OpenSCAD files to that same directory when needed.

## Printables Import Workflow

When the human says to organize or import a model and gives a Printables URL or numeric model id:

1. Extract the model id from the URL, or use the numeric id directly.
2. Query Printables for the model name and file metadata.
3. Derive the directory name with the memory-word-first rule.
4. Create or reuse `~/src/3dprinting/<Model Name>`.
5. Download both pack zips when available:
   `MODEL_FILES` and `PRINT_FILES`
6. Save the original pack zips directly in the model directory.
7. Inspect the zip contents before extraction when there may be filename overlap.
8. If the same path exists in both zips, confirm whether the files are byte-identical.
9. Extract both zips into the same model directory without renaming extracted files.
10. If overlapping files differ, stop and ask the human before overwriting anything.

Practical rules:

- Prefer using the verified operations documented in `printables-api.md`.
- Prefer using the working helpers in `scratch_printables.clj` rather than re-deriving requests from scratch.
- For authenticated Printables operations, stay close to the real browser request shape.
- Keep both upstream zip files after extraction.
- Do not create subdirectories just to separate model files from print files.
- Do not rename `.stl`, `.3mf`, `.gcode`, PDF, or other extracted upstream files.

## Exceptions

Assume there are exceptions in the existing library.

- Watch for local patterns before making sweeping changes.
- Prefer matching established exceptions over forcing everything into one rigid rule.
- Do not try to document every exception in this skill.
