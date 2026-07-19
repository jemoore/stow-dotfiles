# Bash/Linux-specific aliases.
#
# Shared, cross-shell aliases live in common/aliases.sh and are loaded
# BEFORE this file, so anything defined here overrides the shared set.
# (common/aliases.sh runs `unalias -a`, so we must NOT repeat it here.)

# Package management (Arch/Manjaro)
alias pacman='sudo pacman'
alias journalctl='sudo journalctl'
alias pamu='pamac upgrade -a'
alias pamc='pamac checkupdates -a'
alias auru='yay -Syua --noconfirm'
alias se='ls /usr/bin | grep'

export QT_STYLE_OVERRIDE=gtk
export QT_SELECT=qt5

# GNU ls (coreutils) colour options
export __LS_OPTIONS='--color=auto -h'
alias ls='ls $__LS_OPTIONS'
alias ll='ls $__LS_OPTIONS -l'
alias la='ls $__LS_OPTIONS -la'
alias l='ls $__LS_OPTIONS -CF'

# cd
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'

# Disk usage
alias diskusage='df -h'
alias folderusage='du -ch | sort -h'
alias allfolderusage='du -sh | sort -h'
alias sizeof='sudo du -sh'
alias free='free -h'
alias df='df -h'

# System (Arch/systemd)
alias progs="(pacman -Qet && pacman -Qm) | sort -u" # List programs I've installed
alias orphans='pacman -Qdt'                         # List orphan programs
alias rmorphans='pacman -Rns $(pacman -Qtdq)'
alias sdn="sudo shutdown now"
alias hibernate='sudo systemctl hibernate'
alias restart='sudo systemctl restart'
alias scale='xrandr --output eDP1 --scale 0.85x0.85'
alias xup='xrdb ~/.Xresources'
alias packages="pacman -Qqetn" # installed packages omit dependencies and AUR
alias foreign="pacman -Qqem"   # installed AUR (or other) packages
alias rmpackage='pacman -Rns'  # remove package and all dependencies not used by others

# Misc
alias gdev='cd /mnt/data/dev'
alias gkb='cd /mnt/data/Documents/kb'
alias valias='vim $HOME/dev/github.com/jemoore/stow-dotfiles/bash/.local/shell.d/bash/aliases.bash'
alias vn='vim $HOME/Documents/mdnotes/MOC.md'
alias home='cd ~'
alias mkdir='mkdir -pv'
alias mv='mv -iv'
alias rm='rm -Iv --one-file-system --preserve-root'

alias myip='ip -br -c a && (echo "external " && curl checkip.amazonaws.com)'
alias weather="curl wttr.in/"
alias shredit="shred -n 5 -u -z"
alias cbg='feh --recursive --randomize --bg-scale /mnt/data/Documents/wallpaper/Bing/*'
alias dlayout='$SCRIPTS/desk-screenlayout.sh'
alias pydev='source $SCRIPTS/pydev'
alias pipupgrade='python -m pip install --upgrade pip'

# GNU grep colour
alias grep='grep -i --colour=auto'
alias egrep='egrep -i --colour=auto'
alias fgrep='fgrep -i --colour=auto'
alias curl='curl -L'

# Fun but useless
alias fclock="watch -n1 \"date '+%D%n%T'|figlet -k\""
alias figwatch='watch -n1 "date '+%D%n%T'|figlet -k"'
alias pfortune='fortune | ponysay'
alias cfortune='fortune | cowsay'
alias w='nitrogen --set-zoom-fill --random /mnt/data/Documents/wallpaper/Bing'

alias '?'=duck
alias '??'=google
alias '???'=bing
alias x="exit"
alias sl="sl -e"
alias mkdirisosec='d=$(isosec);mkdir $d; cd $d'

which vim &>/dev/null && alias vi=vim

# fzf helpers (Linux clipboard via xclip)
alias cpcmd="history | cut -c 8- | uniq | fzf | xclip -i -r -sel clipboard"
alias c='file=$(rg --files --hidden | fzf | sed "s~/[^/]*$~/~");[[ "$file" == "" ]]|| cd "$file"'

# remember, instead of alias use cd `...`
# so alias tmpd='cd $(mktemp -d)' just becomes cd `mktemp -d`

# backup helper (kept as a note):
# backup() { rsync --relative --force --ignore-errors --no-perms --chmod=ugo=rwX \
#   --delete --backup --backup-dir=$(date +%Y%m%d-%H%M%S)_Backup --whole-file -a -v "$1"/ ~/Backup; }
