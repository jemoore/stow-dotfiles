# Usage

The `--no-folding` flag instructs Stow to only create symbolic links for
individual files (the "leaves") and not "fold" entire subtrees into a single
directory symlink. This achieves the desired result of linking only individual
files to your home directory while preserving the folder structure in your
dotfiles repository.

## Shell configuration layout

Shell config is split into three layers under `~/.local/shell.d/`:

- `common/` — POSIX `sh`, sourced by **both** bash and zsh (aliases, functions,
  PATH helpers, platform detection, cross-platform exports).
- `bash/`   — bash-only (`shopt`, bash completion, `PS1`, GNU/Linux aliases).
- `zsh/`    — zsh-only (oh-my-zsh add-ons, macOS/BSD `ls` colours).

`~/.bashrc` and `~/.zshrc` each source `common/` first, then their own layer.
Adding a new file to `common/` picks it up in both shells with no rc edits.

## Stowing

Packages common to every machine:

```
stow --no-folding -t $HOME common alacritty emacs git scripts tmux vim vscode
```

Then the shell package for the host's login shell:

```
# Linux (bash)
stow --no-folding -t $HOME bash

# macOS (zsh)
stow --no-folding -t $HOME zsh
```
