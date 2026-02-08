unalias -a

alias pacrepo='sudo reflector -l 20 -f 10 --save /etc/pacman.d/mirrorlist'
alias pacman='sudo pacman'
alias journalctl='sudo journalctl'
# alias pacu='sudo pacman -Syu --noconfirm'
alias pamu='pamac upgrade -a'
alias pamc='pamac checkupdates -a'
alias auru='yay -Syua --noconfirm'
# alias systemctl='sudo systemctl'
alias se='ls /usr/bin | grep'

export QT_STYLE_OVERRIDE=gtk
export QT_SELECT=qt5

if [[ $LANG = '' ]]; then
	export LANG=en_US.UTF-8
fi


export __LS_OPTIONS='--color=auto -h'

# alias vim='nvim'
# fedora vim package does not have clipboard support
# alias vim='gvim -v'
alias e='emacsclient -c -a "emacs"&'
alias emacs='emacsclient -c -a "emacs"&'
alias bemacs='systemctl stop --user emacs && sleep 5 && systemctl start --user emacs'

alias ls='ls $__LS_OPTIONS'
alias ll='ls $__LS_OPTIONS -l'
alias la='ls $__LS_OPTIONS -la'
alias l='ls $__LS_OPTIONS -CF'
alias sl='ls $__LS_OPTIONS'

# cd
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'

# Git version control
alias gs='git status'
alias gp='git push -u origin master'
alias glazy='git add . && git commit -m '$1' && git push -u origin master'

git-grep(){
  local sstring="$1"
  vim -c ':cgetexpr system("git grep --break --line-number "$sstring"")' -c ':copen'
}
alias gg='git-grep '

# Disk usage
alias diskusage='df -h'
alias folderusage='du -ch | sort -h'
alias allfolderusage='du -sh | sort -h'
alias sizeof='sudo du -sh'

#system
alias progs="(pacman -Qet && pacman -Qm) | sort -u" # List programs I've installed
alias orphans='pacman -Qdt' # List orphan programs
alias rmorphans='pacman -Rns $(pacman -Qtdq)'
alias sdn="sudo shutdown now"
alias hibernate='sudo systemctl hibernate'
alias restart='sudo systemctl restart'
alias scale='xrandr --output eDP1 --scale 0.85x0.85'
alias xup='xrdb ~/.Xresources'
alias packages="pacman -Qqetn" # installed packages omit dependencies and AUR
alias foreign="pacman -Qqem" # installed AUR (or other)  packages
alias rmpackage='pacman -Rns' # remove package and all dependencies not used by others
#misc
alias gdev='cd /media/data/dev'
alias gkb='cd /media/data/Documents/kb'
alias valias='vim $HOME/dev/github.com/jemoore/dotfiles/shell.d/aliases.sh'
alias v3='vim $HOME/dev/github.com/jemoore/dotfiles/i3/config'
alias vn='vim /mnt/data/Documents/md/kb/MOC.md'
alias home='cd ~'
alias mkdir='mkdir -pv'
alias mv='mv -iv'
alias rm='rm -Iv --one-file-system --preserve-root'
alias yt="yt-dlp --add-metadata -ic" # Download video link
alias yta='yt-dlp --extract-audio --audio-format mp3 '

alias myip='ip -br -c a && (echo "external " && curl checkip.amazonaws.com)'

alias reseti3="i3-msg reload && i3-msg restart"
alias mntwindows="sudo mount /dev/sda3 /mnt/Windows"
alias weather="curl wttr.in/"

alias shredit="shred -n 5 -u -z"

alias cbg='feh --recursive --randomize --bg-scale /mnt/data/Documents/wallpaper/Bing/*'
alias dlayout='$HOME/dev/repos/github/jemoore/dotfiles/desk-screenlayout.sh'

alias pydev='source ~/.local/bin/scripts/pydev'
alias pipupgrade='python -m pip install --upgrade pip'
alias ppath='printf "${PATH//:/\\n}"'

# Fun but useless
alias fclock="watch -n1 \"date '+%D%n%T'|figlet -k\""
alias figwatch='watch -n1 "date '+%D%n%T'|figlet -k"'
alias pfortune='fortune | ponysay'
alias cfortune='fortune | cowsay'
alias w='nitrogen --set-zoom-fill --random /mnt/data/Documents/wallpaper/Bing'

# following seems usefull, not sure where to put yet
#backup() { source=$1 ; rsync --relative --force --ignore-errors --no-perms --chmod=ugo=rwX --delete --backup --backup-dir=$(date +%Y%m%d-%H%M%S)_Backup --whole-file -a -v $source/ ~/Backup ; } ; backup /source_folder_to_backupias grep='grep -i --colour=auto'

alias egrep='egrep -i --colour=auto'
alias fgrep='fgrep -i --colour=auto'
alias curl='curl -L'
alias ls='ls -h --color=auto'
alias '?'=duck
alias '??'=google
alias '???'=bing
alias x="exit"
alias sl="sl -e"
alias mkdirisosec='d=$(isosec);mkdir $d; cd $d'

# remember, instead of alias use cd ``
# so alias tmpd='cd $(mktemp -d)'
# just becomes cd `mktemp -d`

alias free='free -h'
alias df='df -h'
alias top="htop"
alias dev='cd /mnt/data/dev'

which vim &>/dev/null && alias vi=vim

# aliases for fzf
export FZF_DEFAULT_COMMAND="rg --files --hidden"
alias cpcmd="history | cut -c 8- | uniq | fzf | xclip -i -r -sel clipboard"
alias c='file=$(rg --files --hidden | fzf | sed "s~/[^/]*$~/~");[[ "$file" == "" ]]|| cd "$file"'

# aliases for fzf searching and opening files
## ft and ff are custom bash functions defined in funcs.sh
## that uses fzf to find files and open them in
## vim/nvim or system default app based on MIME type
alias fn='ft /mnt/data/Documents/md/kb'
alias ffn='ff /mnt/data/Documents/md/kb'
alias fb='ft /mnt/data/Documents/Books'
alias ffb='ff /mnt/data/Documents/Books'

# doom emacs
export PATH="$PATH:$HOME/.emacs.d/bin/"

