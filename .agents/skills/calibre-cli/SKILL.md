---
name: calibre-cli
description: Use Calibre CLI tools (calibredb, fetch-ebook-metadata) to search, fix, and improve your eBook library metadata
---

# Calibre CLI

Use this skill to inspect and maintain the Calibre library on `dewey`.

## Local entrypoints

Use `./scripts/calibredb ...` from the repo root for all `calibredb` work. The examples below use bare `calibredb` syntax to match upstream docs, but in this repo the wrapper is the default entrypoint. `./scripts/calibredb` SSHes to `dewey` and runs `calibredb` inside the `calibre` container.

The wrapper is not a dumb pass-through:

- If no library is specified, it defaults to the live library at `/media/books`.
- If direct access hits the Calibre live-library lock, it retries automatically through the local Calibre Content server.
- It discovers the current content-server URL and credentials from the running container.
- `./scripts/calibredb --wrapper-info` prints the discovered library path, server URL, auth mode, username, and password.

Use `./scripts/fetch-ebook-metadata ...` from the repo root for metadata lookups. The examples below use bare `fetch-ebook-metadata` syntax to match upstream docs, but in this repo the wrapper is the default entrypoint. `./scripts/fetch-ebook-metadata` SSHes to `dewey` with an `EOF` heredoc and runs `fetch-ebook-metadata` inside the same `calibre` container.

Example:

```bash
./scripts/fetch-ebook-metadata -t "The Great Gatsby" -a "F. Scott Fitzgerald"
```

To save OPF output locally, redirect the wrapper output on the local side:

```bash
./scripts/fetch-ebook-metadata -t "1984" -a "George Orwell" -o > /tmp/metadata.opf
```

## Locked library and content server mode

If direct `calibredb --library-path ...` access fails with:

```text
Another calibre program such as calibre-server or the main calibre program is running.
```

the wrapper should retry automatically through the Calibre Content server. Reach for manual `--with-library` only when you need to override the default behavior or inspect the connection details yourself.

For this setup on `dewey`, the active library is usually available from inside the container as:

```bash
http://127.0.0.1:8081/#books
```

If the content server has authentication enabled, `calibredb` also needs `--username` and `--password`. The skill should not assume anonymous access when using `--with-library`.

To inspect the auto-discovered connection details:

```bash
./scripts/calibredb --wrapper-info
```

Manual example:

```bash
./scripts/calibredb \
  --with-library "http://127.0.0.1:8081/#books" \
  --username "$CALIBRE_USER" \
  --password "$CALIBRE_PASS" \
  show_metadata 42
```

## Safety

Start with inspection commands before changing anything:

- `calibredb search ...`
- `calibredb list -s "..." -f id,title,authors`
- `calibredb show_metadata ID`
- `calibredb check_library`
- `fetch-ebook-metadata -o`

Permanent removals and broad write operations require explicit human approval. That includes `remove`, `remove --permanent`, multi-book `set_metadata`, `embed_metadata all`, `backup_metadata --all`, bulk `add`, and bulk `add_format`.

Before any destructive or bulk change, do a dry-run style check first so the target set is visible. Usually that means:

1. Search for the candidate IDs.
2. List the affected books with `id`, `title`, and `authors`.
3. Inspect one or two representative books with `show_metadata`.
4. Only then run the write command.

## Available CLI tools

### `fetch-ebook-metadata`

Fetches book metadata from online sources such as Google, Amazon, and Open Library.

Useful options:

- `-t "Title"` for title
- `-a "Author Name"` for author
- `-i ISBN` for ISBN lookup
- `-I identifier:value` for other identifiers such as `asin` or `goodreads`
- `-p PLUGIN` to limit lookup to a specific source
- `-c /path/to/cover.jpg` to save a cover image
- `-o` to emit OPF XML
- `-v` for verbose search output
- `-d SECONDS` to change the timeout

Examples:

```bash
fetch-ebook-metadata -t "The Great Gatsby" -a "F. Scott Fitzgerald"
fetch-ebook-metadata -i 9780743273565
fetch-ebook-metadata -t "Dune" -a "Frank Herbert" -c cover.jpg
fetch-ebook-metadata -t "1984" -a "George Orwell" -o > metadata.opf
```

### `calibredb`

Manages the Calibre database: list, search, add, remove, and update metadata.

Global options:

- `--library-path /path/to/library`
- `--with-library URL`

In this repo, the wrapper and container defaults are usually enough, so prefer the local wrapper unless there is a specific reason to override library selection.

#### Core commands

`calibredb list`

```bash
calibredb list
calibredb list -f title,authors,isbn,formats
calibredb list -s "author:asimov"
calibredb list --for-machine
calibredb list --sort-by title
calibredb list -s "isbn:false"
```

`calibredb search`

```bash
calibredb search "title:gatsby"
calibredb search "author:asimov 'series:foundation'"
calibredb search "formats:epub and not formats:mobi"
calibredb search "date:>2020"
calibredb search "rating:>=4"
```

`calibredb show_metadata ID`

```bash
calibredb show_metadata 42
calibredb show_metadata 42 --as-opf
```

`calibredb set_metadata ID [opf_file]`

```bash
calibredb set_metadata 42 metadata.opf
calibredb set_metadata 42 --field "title:Correct Title"
calibredb set_metadata 42 --field "authors:First Last"
calibredb set_metadata 42 --field "isbn:9780743273565"
calibredb set_metadata 42 --field "tags:fiction,classic,must-read"
calibredb set_metadata 42 --field "series:Foundation" --field "series_index:1"
calibredb set_metadata 42 --field "identifiers:isbn:123,asin:B00ABC,goodreads:456"
calibredb set_metadata --list-fields
```

`calibredb add`

```bash
calibredb add book.epub
calibredb add book.epub -t "Title" -a "Author"
calibredb add -r /path/to/folder
calibredb add -e
calibredb add book.epub -m ignore
```

`calibredb remove`

```bash
calibredb remove 42
calibredb remove 1,5,10-15
calibredb remove 42 --permanent
```

`calibredb add_format ID file`

```bash
calibredb add_format 42 book.mobi
```

`calibredb export IDs`

```bash
calibredb export 42 --to-dir /exports
calibredb export 1,2,3 --formats epub,mobi --to-dir /exports
```

#### Library health commands

`calibredb check_library`

```bash
calibredb check_library
calibredb check_library -r missing_covers
calibredb check_library -r missing_formats
calibredb check_library --csv
```

Available report names include `invalid_titles`, `extra_titles`, `invalid_authors`, `extra_authors`, `missing_formats`, `extra_formats`, `extra_files`, `missing_covers`, `extra_covers`, `malformed_formats`, `malformed_paths`, and `failed_folders`.

`calibredb backup_metadata`

```bash
calibredb backup_metadata
calibredb backup_metadata --all
```

`calibredb embed_metadata ID`

```bash
calibredb embed_metadata 42
calibredb embed_metadata all
calibredb embed_metadata 1 2 10-15
```

#### Full-text search

```bash
calibredb fts_index status
calibredb fts_index enable
calibredb fts_search "search terms"
calibredb fts_search "term" --include-snippets
```

## Metadata fetching strategies

### ISBN-first

If you have an ISBN, start there.

```bash
fetch-ebook-metadata -i 9780743273565 -o > /tmp/meta.opf
calibredb set_metadata BOOK_ID /tmp/meta.opf
```

### Title and author cleanup

Titles often need cleaning for successful searches. Try multiple variations:

1. Remove subtitles after `:` or `-`.
2. Remove edition text such as `2nd Edition` or `Revised`.
3. Remove series text in parentheses or brackets.
4. Try the first few significant words only.
5. Remove leading articles such as `The`, `A`, or `An`.

For authors:

1. Try `First Last`.
2. Try last name only.
3. Remove honorifics or suffixes.
4. For multiple authors, try the first author only.

Example workflow:

```bash
fetch-ebook-metadata -t "The Great Gatsby: A Novel" -a "F. Scott Fitzgerald" -v
fetch-ebook-metadata -t "The Great Gatsby" -a "F. Scott Fitzgerald" -v
fetch-ebook-metadata -t "Great Gatsby" -a "Fitzgerald" -v
fetch-ebook-metadata -t "Great Gatsby" -a "Fitzgerald" -p "Open Library" -v
fetch-ebook-metadata -t "Great Gatsby" -a "Fitzgerald" -p "Google" -v
```

### ASIN or Goodreads lookup

```bash
fetch-ebook-metadata -I asin:B0BXN2D4SH -v
fetch-ebook-metadata -I goodreads:12345678 -v
```

## Common workflows

### Fix books with missing metadata

```bash
calibredb list -s "isbn:false" -f id,title,authors
calibredb show_metadata BOOK_ID
fetch-ebook-metadata -t "Clean Title" -a "Author" -o > /tmp/meta.opf
cat /tmp/meta.opf
calibredb set_metadata BOOK_ID /tmp/meta.opf
calibredb set_metadata BOOK_ID --field "isbn:9780123456789"
```

### Download missing covers

```bash
calibredb check_library -r missing_covers
fetch-ebook-metadata -t "Title" -a "Author" -c /tmp/cover.jpg
```

### Mark a book as read

Default interpretation for this library:

- If the human says "read date", treat that as setting both `#read_first_date` and `#read_last_date` to the same date unless they explicitly say otherwise.
- Marking a book as read means adding the `read` tag and setting the read date fields.
- Do not use `#percent_read` to mark a completed book unless the human explicitly asks for it.
- Preserve the book's existing tags when adding `read`.

Inspect first:

```bash
./scripts/calibredb search 'title:"The Tomb of Dragons" and author:"Katherine Addison"'
./scripts/calibredb show_metadata 3921
./scripts/calibredb show_metadata 3921 --as-opf
```

Apply the read state:

```bash
./scripts/calibredb set_metadata 3921 \
  --field "#read_first_date:2026-04-17" \
  --field "#read_last_date:2026-04-17" \
  --field "tags:Fiction,read"
```

The custom fields used for this workflow are:

- `#read_first_date`
- `#read_last_date`
- `#percent_read`

### Clean up duplicate authors

Common issue: `Asimov, Isaac` vs `Isaac Asimov` vs `asimov, isaac`

```bash
calibredb search "author:\"Asimov, Isaac\""
calibredb set_metadata ID --field "authors:Isaac Asimov"
```

### Add series information

```bash
calibredb set_metadata 42 --field "series:Foundation" --field "series_index:1"
calibredb search "author:asimov"
calibredb list -s "author:asimov" -f id,title,series,series_index
```

### Health check and repair

```bash
calibredb check_library
calibredb check_library -r missing_formats,missing_covers,malformed_paths
calibredb backup_metadata --all
calibredb embed_metadata all
```

## Search query syntax

Common query forms:

- `title:word`
- `title:"exact phrase"`
- `author:name`
- `series:name`
- `tags:fiction`
- `formats:epub`
- `rating:>=4`
- `date:>2020`
- `isbn:true` or `isbn:false`
- `cover:true` or `cover:false`
- `not QUERY`
- `QUERY1 and QUERY2`
- `QUERY1 or QUERY2`

Examples:

```bash
calibredb search "author:asimov series:foundation"
calibredb search "formats:epub and not formats:mobi"
calibredb search "isbn:false and author:known"
calibredb search "tags:fiction rating:>=4 date:>2020"
```

## Metadata fields

Standard fields for `--field`:

- `title`
- `authors`
- `author_sort`
- `publisher`
- `pubdate`
- `series`
- `series_index`
- `rating`
- `tags`
- `comments`
- `isbn`
- `identifiers`
- `languages`

List custom columns with:

```bash
calibredb custom_columns -d
```

## Command hygiene

- Quote arguments that contain spaces.
- Use `--for-machine` when you need stable output for parsing.
- Capture OPF output and inspect it before applying metadata changes.
- Use verbose mode when metadata lookup is failing or ambiguous.
- Work in small batches for bulk operations.
- Verify changes with `calibredb show_metadata BOOK_ID` after writes.

## Troubleshooting

If `fetch-ebook-metadata` returns no results:

- Clean up the title.
- Try the author last name only.
- Try a specific plugin.
- Check for typos.
- Accept that some books are not indexed online.

If `calibredb` reports a locked library:

- Check for another active Calibre or `calibredb` process.
- Assume the library may be busy before retrying writes.

If metadata matches the wrong book:

- Review the OPF before applying it.
- Prefer ISBN lookups when possible.
- Confirm publication year, author, and series data before writing.
