# Dotfile Management

This repo uses `stow` to manage dot files on my Linux and Mac systems.
The files stay in this repo and the `stow` command creates links in the
home directory, in the appropriate location, back to files in this repo.

## Shell configuration layout

Shell config is split into three layers under `~/.local/shell.d/`:

- `common/` — POSIX `sh`, sourced by **both** bash and zsh (aliases, functions,
  PATH helpers, platform detection, cross-platform exports).
- `bash/`   — bash-only (`shopt`, bash completion, `PS1`, GNU/Linux aliases).
- `zsh/`    — zsh-only (oh-my-zsh add-ons, macOS/BSD `ls` colours).

`~/.bashrc` and `~/.zshrc` each source `common/` first, then their own layer.
Adding a new file to `common/` picks it up in both shells with no rc edits.

## Emacs

The `emacs` package targets `~/.config/emacs/` (XDG), not `~/.emacs.d/`.
This is deliberate: Emacs only falls back to `~/.config/emacs` when
`~/.emacs.d/` and `~/.emacs` are both absent (see `startup--xdg-or-homedot`
in `startup.el`), and Omarchy's Emacs integration lives in `~/.config/emacs/`.
Creating `~/.emacs.d` at all would silently take the whole config away from it.

`init.el` and `early-init.el` are hand-written and tracked -- they are no
longer tangled from an Org file. On a new machine, stow the package and start
Emacs; packages install themselves on first launch. Note that this overwrites
the stock `init.el` that `omarchy-emacs-setup` drops there.

Stowed files: `init.el`, `early-init.el`, `private.el`, `custom-emacs/`.
`private.el` holds machine-local path overrides only; macOS needs none, since
`init.el`'s defaults already match it. `custom.el` is written by Customize and
is deliberately untracked.

One-time setup per machine:

- `M-x jem/treesit-install-grammars` -- builds the tree-sitter grammars
  (needs `git` and a C compiler). Until this runs, Go and Rust files have no
  major mode at all; `init.el` says so at startup.
- `M-x nerd-icons-install-fonts`, then restart Emacs.
- Language servers, installed outside Emacs: `clangd`, `pylsp`, `gopls`,
  `rust-analyzer`. Common Lisp uses `sly` + `sbcl`, not LSP.
- Spellcheck needs `aspell` (`brew install aspell`); without it flyspell stays
  off rather than erroring.

`archived-Emacs.org` is the previous literate config, kept for reference until
the new `init.el` has proven itself. It is not loaded or tangled.

**Known gap:** the Omarchy desktop integration described in
`archived-Emacs.org` has *not* been ported to the new `init.el`, so on Omarchy
the theme/font sync and `omarchy.el` load do not happen yet.

The remaining files in `~/.config/emacs/` (`omarchy.el`, `shell-bashrc`,
`themes/omarchy-theme.el`) belong to the `omarchy-emacs` package and are left
alone. If `omarchy-emacs-setup` is ever re-run it will warn about `~/.emacs.d`;
there is nothing to answer, that directory no longer exists.

## Stowing

The `--no-folding` flag instructs Stow to only create symbolic links for
individual files (the "leaves") and not "fold" entire subtrees into a single
directory symlink. This achieves the desired result of linking only individual
files to your home directory while preserving the folder structure in your
dotfiles repository.

Packages common to every machine:

```
stow --no-folding -t $HOME common alacritty emacs git kitty scripts tmux nvim vim
```

Then the shell package for the host's login shell:

```
# Linux (bash)
stow --no-folding -t $HOME bash

# macOS (zsh)
stow --no-folding -t $HOME zsh
```

## OS-specific packages

Some applications expect their config in a different place on each OS, so those
packages live under `linux/` and `macos/` and are stowed with `-d`:

```
# Linux
stow --no-folding -d linux -t $HOME vim vscode

# macOS
stow --no-folding -d macos -t $HOME vim tmux vscode
```

### vscode

VS Code reads its user config from `~/.config/Code/User` on Linux but from
`~/Library/Application Support/Code/User` on macOS. The real `settings.json` and
`keybindings.json` are kept once, in the top-level `vscode` package; the
`linux/vscode` and `macos/vscode` packages contain relative symlinks back to
them, so stow lands the files in the right place per OS without duplicating
content. Do not stow the top-level `vscode` package directly.
