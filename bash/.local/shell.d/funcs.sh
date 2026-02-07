# just a few random functions to try out


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

mem()
{
  ps -eo rss,pid,euser,args:100 --sort %mem | grep -v grep | grep -i $@ | awk '{printf $1/1024 "MB"; $1=""; print }'
}

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

ft() {
  # $1 should be a directory
  if [ -d $1 ]; then
    local file
    # Find files in the directory and pipe them to fzf for selection.
    file=$(find $1 -type f -not -path '*/.obsidian/*' | fzf)

    # If a file was selected, open it in vim.
    if [[ -n "$file" ]]; then
      # Check if nvim is available and use it; otherwise, use vim.
      if command -v nvim &> /dev/null; then
        nvim "$file"
      else
        vim "$file"
      fi
    fi
  else
      echo "Directory does not exist: " $1
  fi
}
