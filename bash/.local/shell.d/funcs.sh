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
# opens text files in vim/nvim, others with xdg-open
ft() {
  # $1 should be a directory
  if [ -d $1 ]; then
    local file
    # Find files in the directory and pipe them to fzf for selection.
    file=$(find $1 -type f -not -path '*/.obsidian/*' -not -path '*/.git/*' | fzf)

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
  else
      echo "Directory does not exist: " $1
  fi
}
