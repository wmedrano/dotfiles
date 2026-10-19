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
link "$repo/bash/bashrc" "$HOME/.bashrc"
link "$repo/git/ignore" "$HOME/.config/git/ignore"

install_dumile_ca() {
    local crt="$repo/certs/dumile-local-root.crt"
    if openssl verify -CAfile /etc/ssl/certs/ca-certificates.crt "$crt" >/dev/null 2>&1; then
        echo "ok: dumile.local CA trusted"
    else
        sudo trust anchor "$crt"
        echo "installed: dumile.local CA"
    fi
}

domain="${DOMAIN:-$(hostname -d 2>/dev/null || true)}"
host="${HOSTNAME:-$(hostname 2>/dev/null || true)}"
if [[ "${domain,,}" == *.com || "${host,,}" == *.com ]]; then
    echo "skip: .pi and certificate setup (*.com host)"
else
    link "$repo/pi" "$HOME/.pi"
    link "$repo/environment.d/node-extra-ca.conf" "$HOME/.config/environment.d/node-extra-ca.conf"
    install_dumile_ca
fi
