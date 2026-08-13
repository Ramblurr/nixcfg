---
name: recipe-card
description: Create compact printable recipe cards from recipe pages or recipe text. Use when asked to add, format, print, regenerate, or share a recipe.
---

# Recipe Card

Use `~/src/github.com/ramblurr/playground/recipe-cards` as the project directory. Store recipe JSON and generated PDFs in `out/`. Git ignores that directory.

When the user gives a recipe URL, get its print page first. If the URL is not a print page, find the print link or try the site's print URL pattern. If the user gives recipe text without a URL, use that text and record any source details they provide. If the user does not provide the print page, find it yourself.

Fetch the print page with `curl`, redirects enabled, and a normal browser user-agent. If curl is blocked or returns incomplete content, load the web-browser skill and use a browser. Prefer the print page because it usually contains the complete ingredients, directions, timing, yield, author, notes, and equipment without article clutter.

Read the source and create `out/<recipe-slug>.json` by following an existing JSON file. Preserve quantities, directions, timing, yield, author, source, notes, and equipment. Fix broken whitespace and typography, but do not alter the recipe. Set `source_url` to the canonical recipe page without print query parameters.

Generate the PDF from the project directory:

```console
./recipe_card.py out/<recipe-slug>.json
```

Verify the JSON parses and the PDF has one A4 page. Extract its text to confirm every ingredient, direction, and important note appears. Render the top half to an image and inspect it. Check that nothing overlaps or overflows and that the author and source appear beside the title, where trimming will not remove them.

When the user needs the PDF over HTTP on Quine, serve only `out/` on the Prim address. Find the current IPv4 address instead of assuming it:

```console
prim_ip=$(ip -4 -o addr show dev prim | awk '{sub(/\/.*/, "", $4); print $4}')
```

Quine's firewall allows development ports `3000` through `3050` for both TCP and UDP. Choose a free TCP port in that range for the HTTP server. Check current listeners and the active firewall rules before choosing the port. If the active rules do not include the range, tell the user that the updated NixOS configuration still needs deployment; do not change the live firewall directly.

Start a temporary server in the background, replacing `<port>` with the chosen port:

```console
setsid python3 -m http.server <port> --bind "$prim_ip" --directory "$HOME/src/github.com/ramblurr/playground/recipe-cards/out" </dev/null >/tmp/recipe-card-http.log 2>&1 &
```

Confirm that the process listens only on the Prim address. Download the PDF with curl and compare it byte-for-byte with the file in `out/`. Give the user the direct URL:

```text
http://<prim-ip>:<port>/<recipe-slug>.pdf
```

If replacing an earlier recipe-card server, stop only the process you identified as that server. Leave unrelated listeners alone.
