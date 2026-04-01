# just a few random functions to try out


# Display comprehensive system status including uptime, disk space, memory, and logs
status() {
  { echo -e "\nuptime:"
    uptime
    echo -e "\ndisk space:"
    df -h 2> /dev/null
    echo -e "\ninodes:"
    df -i 2> /dev/null
    echo -e "\nblock devices:"
    blkid
    echo -e "\nmemory:"
    free -m
    if [[ -r /var/log/syslog ]]; then
      echo -e "\nsyslog:"
      tail /var/log/syslog
    fi
    if [[ -r /var/log/messages ]]; then
      echo -e "\nmessages:"
      tail /var/log/messages
    fi
  } | less
}

# Search for processes by name and display memory usage in MB
mem()
{
  ps -eo rss,pid,euser,args:100 --sort %mem | grep -v grep | grep -i $@ | awk '{printf $1/1024 "MB"; $1=""; print }'
}

# Find and open a file from knowledge base directory using fzf
fn() {
  local file
  # Find files in the directory and pipe them to fzf for selection.
  file=$(find /mnt/data/Documents/md/kb -type f -not -path '*/.obsidian/*' | fzf)

  # If a file was selected, open it in vim.
  if [[ -n "$file" ]]; then
    # Check if nvim is available and use it; otherwise, use vim.
    if command -v nvim &> /dev/null; then
      nvim "$file"
    else
      vim "$file"
    fi
  fi
}

# Find and open a file from specified directory using fzf
# Usage: ft <directory> [file_extension]
# Optional file_extension (e.g., md, pdf, org) to limit search to specific file types
# opens text files in vim/nvim, others with xdg-open
ft() {
  if [ -z "$1" ]; then
    echo "Usage: ft <directory> [file_extension]"
    return 1
  fi
  
  if [ ! -d "$1" ]; then
    echo "Directory does not exist: $1"
    return 1
  fi
  
  local dir="$1"
  local file
  
  # Build find command with optional file extension filter
  if [ -n "$2" ]; then
    file=$(find "$dir" -type f -name "*.${2}" -not -path '*/.obsidian/*' -not -path '*/.git/*' | fzf --prompt "*.${2}> ")
  else
    file=$(find "$dir" -type f -not -path '*/.obsidian/*' -not -path '*/.git/*' | fzf)
  fi

  # If a file was selected, open it appropriately.
  if [[ -n "$file" ]]; then
    # Detect MIME type to determine if it's a text file
    local mime_type
    mime_type=$(file --mime-type -b "$file")
    
    # Check if it's a text file or other vim-compatible type
    if [[ "$mime_type" =~ ^text/ ]] || [[ "$mime_type" == "application/json" ]] || \
       [[ "$mime_type" == "application/xml" ]] || [[ "$mime_type" == "application/x-yaml" ]] || \
       [[ "$mime_type" == "application/x-shellscript" ]] || [[ "$mime_type" == "inode/x-empty" ]]; then
      # Check if nvim is available and use it; otherwise, use vim.
      if command -v nvim &> /dev/null; then
        nvim "$file"
      else
        vim "$file"
      fi
    else
      # Use xdg-open for binary/non-text files
      xdg-open "$file" &> /dev/null &
    fi
  fi
}

# Find and open files containing specified text using grep/ripgrep and fzf
# Usage: ff <directory> [file_extension]
# Type search terms in fzf to interactively search file contents
# Optional file_extension (e.g., md, pdf, org) to limit search to specific file types
# opens text files in vim/nvim, others with xdg-open
ff() {
  if [ -z "$1" ]; then
    echo "Usage: ff <directory> [file_extension]"
    return 1
  fi
  
  if [ ! -d "$1" ]; then
    echo "Directory does not exist: $1"
    return 1
  fi
  
  local dir="$1"
  local ext_pattern=""
  local grep_include=""
  
  # Set up file extension filtering if provided
  if [ -n "$2" ]; then
    ext_pattern="-g '*.${2}'"
    grep_include="--include='*.${2}'"
  fi
  
  local file
  # Use ripgrep if available (faster), otherwise fall back to grep
  if command -v rg &> /dev/null; then
    # Interactive search with ripgrep - search updates as you type
    if [ -n "$2" ]; then
      file=$(fzf --disabled --ansi \
        --bind "change:reload:rg --column --line-number --no-heading --color=always --smart-case -g '!.git' -g '!.obsidian' -g '*.${2}' {q} '$dir' 2>/dev/null || true" \
        --bind "start:reload:rg --files --hidden -g '!.git' -g '!.obsidian' -g '*.${2}' '$dir' 2>/dev/null" \
        --delimiter : \
        --preview 'bat --style=numbers --color=always --highlight-line {2} {1} 2>/dev/null || cat {1} 2>/dev/null' \
        --preview-window 'up,60%,border-bottom,+{2}+3/3,~3' \
        --prompt "Search *.${2}> " \
        | cut -d: -f1)
    else
      file=$(fzf --disabled --ansi \
        --bind "change:reload:rg --column --line-number --no-heading --color=always --smart-case -g '!.git' -g '!.obsidian' {q} '$dir' 2>/dev/null || true" \
        --bind "start:reload:rg --files --hidden -g '!.git' -g '!.obsidian' '$dir' 2>/dev/null" \
        --delimiter : \
        --preview 'bat --style=numbers --color=always --highlight-line {2} {1} 2>/dev/null || cat {1} 2>/dev/null' \
        --preview-window 'up,60%,border-bottom,+{2}+3/3,~3' \
        --prompt 'Search> ' \
        | cut -d: -f1)
    fi
  else
    # Fallback to grep-based interactive search
    if [ -n "$2" ]; then
      file=$(fzf --disabled \
        --bind "change:reload:grep -rn --exclude-dir=.git --exclude-dir=.obsidian --include='*.${2}' {q} '$dir' 2>/dev/null | cut -d: -f1 | sort -u || true" \
        --bind "start:reload:find '$dir' -type f -name '*.${2}' -not -path '*/.git/*' -not -path '*/.obsidian/*' 2>/dev/null" \
        --preview 'cat {} 2>/dev/null' \
        --prompt "Search *.${2}> ")
    else
      file=$(fzf --disabled \
        --bind "change:reload:grep -rn --exclude-dir=.git --exclude-dir=.obsidian {q} '$dir' 2>/dev/null | cut -d: -f1 | sort -u || true" \
        --bind "start:reload:find '$dir' -type f -not -path '*/.git/*' -not -path '*/.obsidian/*' 2>/dev/null" \
        --preview 'cat {} 2>/dev/null' \
        --prompt 'Search> ')
    fi
  fi
  
  # If a file was selected, open it appropriately.
  if [[ -n "$file" ]]; then
    # Detect MIME type to determine if it's a text file
    local mime_type
    mime_type=$(file --mime-type -b "$file")
    
    # Check if it's a text file or other vim-compatible type
    if [[ "$mime_type" =~ ^text/ ]] || [[ "$mime_type" == "application/json" ]] || \
       [[ "$mime_type" == "application/xml" ]] || [[ "$mime_type" == "application/x-yaml" ]] || \
       [[ "$mime_type" == "application/x-shellscript" ]] || [[ "$mime_type" == "inode/x-empty" ]]; then
      # Check if nvim is available and use it; otherwise, use vim.
      if command -v nvim &> /dev/null; then
        nvim "$file"
      else
        vim "$file"
      fi
    else
      # Use xdg-open for binary/non-text files
      xdg-open "$file" &> /dev/null &
    fi
  fi
}

# Compression
compress() { tar -czf "${1%/}.tar.gz" "${1%/}"; }
alias decompress="tar -xzf"

# Convert webm files generated by the Gnome screenshot video recorder to mp4s that are more compatible
webm2mp4() {
  input_file="$1"
  output_file="${input_file%.webm}.mp4"
  ffmpeg -i "$input_file" -c:v libx264 -preset slow -crf 22 -c:a aac -b:a 192k "$output_file"
}

# Write iso file to sd card
iso2sd() {
  if [ $# -ne 2 ]; then
    echo "Usage: iso2sd <input_file> <output_device>"
    echo "Example: iso2sd ~/Downloads/ubuntu-25.04-desktop-amd64.iso /dev/sda"
    echo -e "\nAvailable SD cards:"
    lsblk -d -o NAME | grep -E '^sd[a-z]' | awk '{print "/dev/"$1}'
  else
    sudo dd bs=4M status=progress oflag=sync if="$1" of="$2"
    sudo eject $2
  fi
}

# Create a desktop launcher for a web app
web2app() {
  if [ "$#" -ne 3 ]; then
    echo "Usage: web2app <AppName> <AppURL> <IconURL> (IconURL must be in PNG -- use https://dashboardicons.com)"
    return 1
  fi

  local APP_NAME="$1"
  local APP_URL="$2"
  local ICON_URL="$3"
  local ICON_DIR="$HOME/.local/share/applications/icons"
  local DESKTOP_FILE="$HOME/.local/share/applications/${APP_NAME}.desktop"
  local ICON_PATH="${ICON_DIR}/${APP_NAME}.png"

  mkdir -p "$ICON_DIR"

  if ! curl -sL -o "$ICON_PATH" "$ICON_URL"; then
    echo "Error: Failed to download icon."
    return 1
  fi

  cat > "$DESKTOP_FILE" <<EOF
[Desktop Entry]
Version=1.0
Name=$APP_NAME
Comment=$APP_NAME
Exec=google-chrome --app="$APP_URL" --name="$APP_NAME" --class="$APP_NAME" --window-size=800,600
Terminal=false
Type=Application
Icon=$ICON_PATH
Categories=GTK;
MimeType=text/html;text/xml;application/xhtml_xml;
StartupNotify=true
EOF

  chmod +x "$DESKTOP_FILE"
}

web2app-remove() {
  if [ "$#" -ne 1 ]; then
    echo "Usage: web2app-remove <AppName>"
    return 1
  fi

  local APP_NAME="$1"
  local ICON_DIR="$HOME/.local/share/applications/icons"
  local DESKTOP_FILE="$HOME/.local/share/applications/${APP_NAME}.desktop"
  local ICON_PATH="${ICON_DIR}/${APP_NAME}.png"

  rm "$DESKTOP_FILE"
  rm "$ICON_PATH"
}

# Move a reference to a .desktop file, like Spotify.desktop, to a named folder, like Xtra.
# Don't use full path for the .desktop file.
app2folder() {
  if [ "$#" -ne 2 ]; then
    local FOLDERS=$(gsettings get org.gnome.desktop.app-folders folder-children | tr -d "[],'")
    echo "Usage: app2folder <desktop_file.desktop> <folder_name>"
    echo "Folders: $FOLDERS"
    return 1
  fi

  local DESKTOP_FILE="$1"
  local FOLDER="$2"
  local SCHEMA="org.gnome.desktop.app-folders.folder:/org/gnome/desktop/app-folders/folders/$FOLDER/"
  local CURRENT_APPS=$(gsettings get "$SCHEMA" apps)

  if [[ "$CURRENT_APPS" != *"$DESKTOP_FILE"* ]]; then
    local TRIMMED=$(echo "$CURRENT_APPS" | sed "s/^\[//;s/\]$//")
    gsettings set "$SCHEMA" apps "[$TRIMMED, '$DESKTOP_FILE']"
  fi
}

# Rewmove desktop app from folder
app2folder-remove() {
  if [ "$#" -ne 2 ]; then
    local FOLDERS=$(gsettings get org.gnome.desktop.app-folders folder-children | tr -d "[],'")
    echo "Usage: app2folder-remove <desktop_file.desktop> <folder_name>"
    echo "Folders: $FOLDERS"
    return 1
  fi

  local DESKTOP_FILE="$1"
  local FOLDER="$2"
  local SCHEMA="org.gnome.desktop.app-folders.folder:/org/gnome/desktop/app-folders/folders/$FOLDER/"
  local CURRENT_APPS=$(gsettings get "$SCHEMA" apps)

  if [[ "$CURRENT_APPS" == *"$DESKTOP_FILE"* ]]; then
    local RAW_LIST=$(echo "$CURRENT_APPS" | tr -d "[]'")
    IFS=',' read -ra APPS_ARRAY <<< "$RAW_LIST"

    # Filter out the app to be removed
    local NEW_APPS=()
    for app in "${APPS_ARRAY[@]}"; do
      app=$(echo "$app" | xargs) # trim spaces
      if [[ "$app" != "$DESKTOP_FILE" && -n "$app" ]]; then
        NEW_APPS+=("'$app'")
      fi
    done

    # Join list again
    local NEW_LIST=$(IFS=, ; echo "${NEW_APPS[*]}")

    gsettings set "$SCHEMA" apps "[$NEW_LIST]"
  fi
}
