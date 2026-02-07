#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

##############################################################################
# first whatever the system has (required for completion, etc.)
if [ -e /etc/bashrc ]; then
  source /etc/bashrc
fi

SHELL_SCRIPTS_DIR=$HOME/.local/shell.d

source "$SHELL_SCRITPS_DIR/detection.sh"
source "$SHELL_SCRITPS_DIR/path.sh"
source "$SHELL_SCRITPS_DIR/history.bash"
source "$SHELL_SCRITPS_DIR/settings.bash"
source "$SHELL_SCRITPS_DIR/new_prompt.sh"
source "$SHELL_SCRITPS_DIR/colors.bash"
source "$SHELL_SCRITPS_DIR/dircolors.sh"
source "$SHELL_SCRITPS_DIR/completion.bash"
source "$SHELL_SCRITPS_DIR/termcap-colors.sh"
source "$SHELL_SCRITPS_DIR/aliases.sh"
source "$SHELL_SCRITPS_DIR/envx.bash"
source "$SHELL_SCRITPS_DIR/funcs.sh"

# not worried about sharing publicly
test -r ~/.bash_personal && source ~/.bash_personal

# sensitive configurations
test -r ~/.bash_private && source ~/.bash_private

# primarily added for HTTP_PROXY and such
test -r ~/.bash_work && source ~/.bash_work

# Fix for Windows directories are writable by other (o+w) and not sticky.
# Change background to white, more visible for certain terminal themes.
export LS_COLORS="$LS_COLORS:ow=1;37;44:tw=1;37;44:"
##############################################################################

# set XDG_RUNTIME_DIR for systemclt/systemd user commands
[ -z "${XDG_RUNTIME_DIR}" ] && export XDG_RUNTIME_DIR=/run/user/$(id -ru)
[ -z "${DBUS_SESSION_BUS_ADDRESS}" ] && export DBUS_SESSION_BUS_ADDRESS="unix:path=${XDG_RUNTIME_DIR}/bus"

NPM_PACKAGES="${HOME}/.npm-packages"

PATH=$HOME/dev/GitHub/vcpkg:$NPM_PACKAGES/bin:$HOME/.local/bin/i3cmds:$HOME/.local/bin/tools:$PATH
PATH=$HOME/.local/scripts:$PATH
PATH=$HOME/.local/bin:$PATH
export PATH=$PATH

# cuda-toolkit
CUDA_DIR=/usr/local/cuda
if [[ -d ${CUDA_DIR} ]]; then
  export PATH=${CUDA_DIR}/bin:$PATH
  export LD_LIBRARY_PATH=${CUDA_DIR}/lib64:$LD_LIBRARY_PATH
fi

unset MANPATH # delete if you already modified MANPATH elsewhere in your config
export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"

BROWSER=/usr/bin/chromium
EDITOR=/usr/bin/gvim

if [[ -z "${MESA_D3D12_DEFAULT_ADAPTER_NAME}" ]] && [[ "${WSL2_GUI_APPS_ENABLED}" == "1" ]]; then
  export MESA_D3D12_DEFAULT_ADAPTER_NAME=NVIDIA
fi

# powerline-daemon -q
# POWERLINE_BASH_CONTINUATION=1
# POWERLINE_BASH_SELECT=1
# . /usr/lib/python3.7/site-packages/powerline/bindings/bash/powerline.sh

# [[ -f ~/.bashrc.aliases ]] && . ~/.bashrc.aliases

# golang
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"                   # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion

[ -s "$HOME/.cargo/env" ] && \. "$HOME/.cargo/env"

# opencode
export PATH=/home/jeff/.opencode/bin:$PATH
