#
# ~/.zshrc
#

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Theme (see https://github.com/ohmyzsh/ohmyzsh/wiki/Themes)
ZSH_THEME="robbyrussell"

# Plugins. Add wisely -- too many plugins slow down shell startup.
plugins=(git git-prompt)

source $ZSH/oh-my-zsh.sh

# --- User configuration ---------------------------------------------------

# Machine-specific PATH (macOS): rustup installed via Homebrew
if command -v brew >/dev/null 2>&1; then
  export PATH="$(brew --prefix rustup)/bin:$PATH"
fi

# Load shared (bash + zsh) config, then zsh-only config.
# The (N) glob qualifier expands to nothing when a directory is empty,
# so missing/empty dirs are harmless and adding a file never means
# editing this rc.  This runs AFTER oh-my-zsh so our aliases win.
SHELL_D="$HOME/.local/shell.d"
for _f in "$SHELL_D"/common/*.sh(N) "$SHELL_D"/zsh/*.zsh(N); do
  [ -r "$_f" ] && source "$_f"
done
unset _f

# Added by Antigravity
export PATH="/Users/jeff/.antigravity/antigravity/bin:$PATH"
