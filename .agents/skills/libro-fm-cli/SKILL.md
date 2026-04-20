---
name: libro-fm-cli
description: Use the `libro` CLI to log into libro.fm, inspect your library, check downloadable files for a book, and download audiobooks from the shell. Use when working with libro.fm on quine.
---

# Libro.fm CLI

Use `libro` for direct libro.fm access from the shell.

## Commands

- `libro login` stores your email and access token in `~/.config/libro-fm-cli/config.edn` or `$XDG_CONFIG_HOME/libro-fm-cli/config.edn`.
- `libro list` lists books in your library.
- `libro files ISBN` shows available download assets for a book.
- `libro get ISBN DIR` downloads the book into `DIR`, preferring `m4b` and falling back to `mp3`.
- `libro get ISBN DIR --format m4b` or `--format mp3` requires that format.

## Auth

Use `libro login` interactively, or set:

- `LIBRO_FM_EMAIL`
- `LIBRO_FM_PASSWORD`

Environment values override stored config values.

## Structured output

Most commands accept `--json` or `--edn`.

```bash
libro list --edn
libro files 9780063003934 --json
libro get 9780063003934 ./tmp-dl --format m4b
```

If behavior is unclear, inspect `~/src/github.com/ramblurr/libro-fm-cli/README.md`.
