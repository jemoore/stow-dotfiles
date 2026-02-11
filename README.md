# Usage
The --no-folding flag instructs Stow to only create symbolic links for individual files (the "leaves") and not "fold" entire subtrees into a single directory symlink. This achieves the desired result of linking only individual files to your home directory while preserving the folder structure in your dotfiles repository.

```
stow --no-folding -t $HOME alacritty bash emacs git scripts tmux vim vscode
```
