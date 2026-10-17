#!/usr/bin/env bash
# Symlink the configs in this repo into place.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# link SRC DEST: make DEST a symlink to SRC. Refuses to overwrite real files.
link() {
    local src="$1" dest="$2"
    if [[ -L "$dest" && "$(readlink "$dest")" == "$src" ]]; then
        echo "ok: $dest -> $src"
        return
    fi
    if [[ -e "$dest" || -L "$dest" ]]; then
        echo "error: $dest already exists; move it away first" >&2
        return 1
    fi
    mkdir -p "$(dirname "$dest")"
    ln -s "$src" "$dest"
    echo "linked: $dest -> $src"
}

link "$repo/emacs" "$HOME/.emacs.d"
link "$repo/wezterm" "$HOME/.config/wezterm"
