# Cross-platform environment exports (sourced by both bash and zsh).
# Keep only settings that are correct on every platform here; put
# OS/shell-specific values in bash/ or zsh/.

# Default language
: "${LANG:=en_US.UTF-8}"
export LANG

# Preferred editor: prefer nvim, fall back to vim
if command -v nvim >/dev/null 2>&1; then
  export EDITOR=nvim VISUAL=nvim
elif command -v vim >/dev/null 2>&1; then
  export EDITOR=vim VISUAL=vim
fi

# fzf: list files with ripgrep when it is available
if command -v rg >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND="rg --files --hidden"
fi
