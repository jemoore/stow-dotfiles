# Two-line prompt matching the bash prompt (bash/.local/shell.d/bash/prompt.sh):
#
#   ┌ 📂 <cyan>current path</cyan>
#   └ 👤 <green>user@host</green> ( <yellow>git branch</yellow>) $
#
# Loaded after oh-my-zsh, so this overrides the theme's PROMPT.

setopt PROMPT_SUBST

parse_git_branch() {
  local branch
  branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  if [ -n "$branch" ]; then
    # Yellow branch name in parentheses
    echo "(%F{yellow} $branch%f)"
  fi
}

PROMPT='┌ 📂 %F{cyan}%d%f
└ 👤 %F{green}%n@%m%f $(parse_git_branch) %(!.#.$) '

# no right-hand prompt
RPROMPT=''
