unalias -a

# Git version control
alias gs='git status'
alias gp='git push -u origin master'
alias glazy='git add . && git commit -m '$1' && git push -u origin master'

git-grep(){
  local sstring="$1"
  vim -c ':cgetexpr system("git grep --break --line-number "$sstring"")' -c ':copen'
}
alias gg='git-grep '

alias yt='yt-dlp --add-metadata -ic' # Download video link
alias yta='yt-dlp --extract-audio --audio-format mp3 '

alias ppath='printf "${PATH//:/\\n}"'

# aliases for fzf searching and opening files
## ft and ff are custom bash functions defined in funcs.sh
## that use fzf to find files and open them in
## vim/nvim or system default app based on MIME type
alias fn='ft  $HOME/dev/github.com/jemoore/mdnotes'
alias ffn='ff $HOME/dev/github.com/jemoore/mdnotes'
alias fb='ft  $HOME/Documents/Books'
alias ffb='ff $HOME/Documents/Books'

alias ogemma='ollama run gemma4:latest --hidethinking'
alias oqwen='ollama run qwen3.5:9b --hidethinking'

alias bupdate='brew update && brew upgrade'

