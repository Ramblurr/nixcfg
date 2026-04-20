---
name: audiobookshelf-cli
description: Use Babashka and the Audiobookshelf HTTP API to inspect libraries, update audiobook metadata, trigger scans, and manage related audiobook file operations on dewey. Use when working with Audiobookshelf books, series, authors, narrators, imports, or cleanup. Store and read the API key from `~/.config/audiobookshelf-cli/config.edn` under `:api-key` with `0600` permissions.
---

# Audiobookshelf CLI

Use Babashka for all scripting and interactive API work. Keep the workflow Clojure-first.

## Preconditions

- Store and read the API key from `~/.config/audiobookshelf-cli/config.edn`.
- The config must be EDN with the key at `:api-key`.
- Keep the file owner-readable only with mode `0600`.
- Use base URL `https://audiobookshelf.socozy.casa`.
- Audiobooks library ID is `58f1e481-efb7-4b88-ae43-010e921441a7`.
- If you need Babashka or API reference material, use the `extra-reference-material` skill first and `local-git-reference` second.

## Tooling

- Use `bb` for scripts and one-off commands.
- Start a REPL with `bb --nrepl-server 0`, then use `brepl` to evaluate forms.
- Prefer:
  - `babashka.http-client` as `http`
  - `cheshire.core` as `json`

Start most sessions with:

```clojure
(require '[babashka.http-client :as http])
(require '[cheshire.core :as json])
(require '[clojure.edn :as edn])
(require '[clojure.string :as str])

(def base-url "https://audiobookshelf.socozy.casa")
(def config-file
  (str (System/getProperty "user.home") "/.config/audiobookshelf-cli/config.edn"))
(def api-key (:api-key (edn/read-string (slurp config-file))))
(def lib-id "58f1e481-efb7-4b88-ae43-010e921441a7")
(def headers {"Authorization" (str "Bearer " api-key)})
```

All API requests need `Authorization: Bearer <api-key>`.

## Common API operations

Get all libraries:

```text
GET /api/libraries
```

Returns `{:libraries [...]}`.

Get library items:

```text
GET /api/libraries/<lib-id>/items
```

Useful query params include `sort`, `limit`, `page`, `desc`, `filter`, `minified`, `collapseseries`, and `include`. Response shape is `{:results [...] :total N}`.

Get a single item:

```text
GET /api/items/<item-id>
```

Update item metadata:

```text
PATCH /api/items/<item-id>/media
Content-Type: application/json
```

Only send fields you want to change. The server will create or reuse series and author records automatically.

Typical payload shape:

```json
{
  "metadata": {
    "title": "Book Title",
    "series": [
      {
        "name": "Series Name",
        "sequence": "1"
      }
    ]
  }
}
```

Delete an item:

```text
DELETE /api/items/<item-id>
```

Use `isMissing: true` on items to detect books whose files disappeared from disk.

Scan the library:

```text
POST /api/libraries/<lib-id>/scan
```

No body is required. Add query param `force=1` for a full rescan.

## Workflow

For batch metadata updates, always use the same two-step plan/apply process:

1. Build the update data structure once, for example a `def updates` vector of maps with fields like `:id`, `:old-title`, `:new-title`, `:series`, and `:sequence`.
2. Print the plan from that data for user review.
3. Apply changes by iterating over that same `updates` value.

Do not re-derive or re-fetch the target set between plan and apply.

When updating metadata, fix all inferable fields in one pass when practical: title, author, narrator, series, and sequence.

For Libro.fm imports, treat Libro.fm as authoritative for `isbn` and `publisher`.
If existing ABS metadata conflicts on those fields, update ABS to match Libro.fm rather than preserving the prior value.

After writes, verify the updated item with `GET /api/items/<item-id>`.

Use a normal scan after imports or filesystem changes when needed. Treat forced scans as a heavier operation.

## Safety

- Deleting Audiobookshelf items requires explicit human approval.
- Deleting or moving audiobook files on `dewey` requires explicit human approval.
- Forced scans require explicit human approval.
- For batch edits, print the exact plan before applying changes.
- Do not mutate items from a freshly re-derived search result after the user reviewed a different candidate set.

## File operations on dewey

- Reach the server with `ssh dewey <command>`.
- Audiobook library path: `/mnt/mali/tank2/media/misc/Audiobooks/`
- Downloads path: `/mnt/downloads/nzbs/complete/fallback/`
- Author folder convention: `Author Name/YYYY - Book Title`
- Add `(Deutsch)` to the author folder only when the same author has both English and German books. If the author only has German books, use the plain author name.
- Keep filenames clean. Use spaces and avoid dot-separated or weird names.
- Preserve `(Unabridged)` in titles when present.
- Inspect downloads before importing. Watch for archives, nested folders, or unexpected structure.
- Multi-disc books should use `Disc 1`, `Disc 2`, and similar subfolders.
- If a download is a duplicate of something already in the library, confirm that before any deletion and get explicit approval before removing files.

## Sequence numbering

- Main novels use whole numbers such as `1`, `2`, `3`.
- Anthologies and collections use decimal slots by publication position, such as `12.5`.
- Prequels published between main books also use decimal positions, such as `10.5`.

## Example: fix a title and set series

This example finds `Das Rad der Zeit 01 - Drohende Schatten`, renames it to `Drohende Schatten`, and sets the series to `Das Rad der Zeit` with sequence `1`.

```clojure
(require '[babashka.http-client :as http])
(require '[cheshire.core :as json])
(require '[clojure.string :as str])

(def base-url "https://audiobookshelf.socozy.casa")
(def api-key (System/getenv "AUDIOBOOKSHELF_API_KEY"))
(def lib-id "58f1e481-efb7-4b88-ae43-010e921441a7")
(def headers {"Authorization" (str "Bearer " api-key)})

(def all-items
  (:results
   (json/parse-string
    (:body
     (http/get (str base-url "/api/libraries/" lib-id "/items")
               {:headers headers
                :query-params {"sort" "media.metadata.title"
                               "limit" "0"}}))
    true)))

(def book
  (first
   (filter #(str/includes?
             (get-in % [:media :metadata :title] "")
             "Rad der Zeit 01")
           all-items)))

(def update-resp
  (http/patch (str base-url "/api/items/" (:id book) "/media")
              {:headers (assoc headers "Content-Type" "application/json")
               :body
               (json/generate-string
                {:metadata
                 {:title "Drohende Schatten"
                  :series [{:name "Das Rad der Zeit"
                            :sequence "1"}]}})}))

(def verify-resp
  (json/parse-string
   (:body
    (http/get (str base-url "/api/items/" (:id book))
              {:headers headers}))
   true))

(let [meta (get-in verify-resp [:media :metadata])]
  (println "Title:" (:title meta))
  (println "Series:" (:series meta)))
```
