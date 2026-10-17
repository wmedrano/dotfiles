/**
 * Scratch-workspace path helpers for the otto extension.
 */

import { createHash, randomBytes } from "node:crypto";
import { mkdir } from "node:fs/promises";
import { basename, join, relative, resolve, sep } from "node:path";
import { pathToFileURL } from "node:url";
import { getCapabilities, hyperlink } from "@earendil-works/pi-tui";

const ensured = new Set<string>();

export class Scratchy {
  cwd: string;
  dir: string;

  constructor(cwd: string) {
    const hash = createHash("sha256").update(cwd).digest("hex").slice(0, 12);
    const name = basename(cwd).replace(/^\.+/, "") || "root";
    this.cwd = cwd;
    this.dir = join("/tmp/pi/scratch/", `${name}-${hash}`);
  }

  async ensureScratchDir(): Promise<string> {
    if (ensured.has(this.dir)) return this.dir;
    await mkdir(this.dir, { recursive: true });
    ensured.add(this.dir);
    return this.dir;
  }

  contains(path: string): boolean {
    if (path === "") {
      return false;
    }
    const base = resolve(this.dir);
    const abs = resolve(this.cwd, path);
    return abs.startsWith(base + sep);
  }

  relPath(target: string): string | undefined {
    if (target === "") {
      return undefined;
    }
    const base = resolve(this.dir);
    const abs = resolve(this.cwd, target);
    // The trailing separator stops /scratch/abc from also matching /scratch/abcd. The target has
    // to be strictly inside so the scratch dir itself has no name to show. No filesystem check:
    // tool_call fires before the tool runs, so a write target may not exist yet.
    return abs.startsWith(base + sep) ? relative(base, abs) : undefined;
  }

  link(target: string): string {
    if (target === "") {
      const display = `📁 ${this.dir}`;
      const { hyperlinks } = getCapabilities();
      if (!hyperlinks) {
        return display;
      }
      return hyperlink(display, pathToFileURL(this.dir).href);
    }

    // Shorten to the scratch-relative path when possible, regardless of hyperlink
    // support; the hyperlink only makes it clickable.
    const relPath = this.relPath(target);
    const displayPath = relPath === undefined ? target : relPath;
    const { hyperlinks } = getCapabilities();
    if (!hyperlinks) {
      return displayPath;
    }
    // Use a file:// URL: some terminals require a scheme in the OSC 8 target.
    return hyperlink(displayPath, pathToFileURL(resolve(this.cwd, target)).href);
  }

  reserve(path: string, extension: string): string {
    const id = randomBytes(9).toString("base64url");
    const filename = extension ? `${id}.${extension}` : id;
    const relPath = path ? join(path, filename) : filename;
    return join(this.dir, relPath);
  }
}
