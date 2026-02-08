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
