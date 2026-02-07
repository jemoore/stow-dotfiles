
parse_git_branch() {
  branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  if [ -n "$branch" ]; then
    # Yellow text for branch name
    echo -e "(\e[0;33m $branch\e[0m)"
  fi
}

PS1='┌ 📂 \[\e[0;36m\]$PWD\[\e[0m\]\n└ 👤 \[\e[0;32m\]\u@\h\[\e[0m\] $(parse_git_branch) \$ '
