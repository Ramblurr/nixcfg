import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync, writeFileSync, symlinkSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { pathToFileURL } from "node:url";

const { FileFinder } = await import(pathToFileURL(
  join(process.argv[2], "node_modules/@ff-labs/fff-node/dist/index.js"),
));
const root = mkdtempSync(join(tmpdir(), "pi-fff-test-"));
let finder;
const unwrap = (result) => {
  assert.equal(result.ok, true, result.error);
  return result.value;
};
try {
  const workspace = join(root, "workspace");
  const shared = join(root, "shared");
  mkdirSync(workspace);
  mkdirSync(shared);
  writeFileSync(join(workspace, "example.txt"), "fff packaging smoke marker\n");
  writeFileSync(join(shared, "linked.txt"), "symlink smoke marker\n");
  symlinkSync(shared, join(workspace, "docs"));
  finder = unwrap(FileFinder.create({
    basePath: workspace,
    followSymlinks: true,
    disableWatch: true,
  }));
  assert.equal(unwrap(await finder.waitForScan(10000)), true);
  assert(unwrap(finder.fileSearch("example")).items.some(
    (item) => item.relativePath === "example.txt",
  ));
  assert(unwrap(finder.grep("fff packaging smoke marker", { mode: "plain" })).items.some(
    (item) => item.relativePath === "example.txt",
  ));
  assert(unwrap(finder.fileSearch("linked")).items.some(
    (item) => item.relativePath === "docs/linked.txt",
  ));
  console.log("pi-fff: native loading, filename/content search and symlinks passed");
} finally {
  finder?.destroy();
  rmSync(root, { recursive: true, force: true });
}
