---
name: restow-and-test
description: Restow dotfile packages and smoke-test the resulting bash/zsh configuration (aliases, functions, env, prompt rendering). Use after any change to shell config files in this repo.
---

# Restow and test shell config

Stow packages in this repo (`common`, `bash`, `zsh`, `macos`, `linux`, ...) symlink into `$HOME`. Shell fragments live under `~/.local/shell.d/{common,bash,zsh}` and are sourced in filename order (numeric prefixes like `30-env.sh`).

Rules: bash is the Linux shell, zsh is the macOS shell — **after changing one, verify the other stays in sync** (same aliases/functions/prompt style).

## 1. Restow

Always `--no-folding`; use `-R` to drop stale links after renames/deletes:

```bash
REPO=/Users/jeff/dev/github.com/jemoore/stow-dotfiles
stow -d "$REPO" -t /Users/jeff --no-folding -R common zsh
echo "restow exit: $?"
```

If a file was renamed inside a package, `-R` can leave the old symlink behind — `rm -f` the stale link in `~/.local/shell.d/...` then restow.

## 2. Syntax check (before launching a shell)

```bash
zsh -n path/to/file.zsh && echo ok
bash -n path/to/file.sh && echo ok
```

## 3. Smoke test the interactive shell

```bash
zsh -i -c '
echo "ft:      $(type ft 2>/dev/null | head -1)"
echo "status:  $(type status 2>/dev/null | head -1)"
echo "gs alias: $(alias gs 2>/dev/null)"
echo "ls alias: $(alias ls 2>/dev/null)"
echo "EDITOR=$EDITOR  LANG=$LANG  PLATFORM=$PLATFORM"
' 2>&1
```

Anything printed *above* the first echo is a startup error — investigate it. Same pattern with `bash -i -c` for the bash side.

## 4. Verify prompt rendering

Render inside and outside a git repo (branch segment should appear only inside):

```bash
zsh -i -c 'cd '"$REPO"' && print -P "$PROMPT"' 2>&1   # inside a git repo
zsh -i -c 'cd /tmp && print -P "$PROMPT"' 2>&1        # outside
```

For bash, `bash -i -c 'cd ...; echo "${PS1@P}"'`.

## Debugging glyph/unicode issues

Prompt symbols (nerd-font branch glyph etc.) that "don't render" are often byte-level problems. Compare the bash and zsh sources with visible bytes:

```bash
grep 'branch' <file> | cat -v
grep 'branch' <file> | hexdump -C | head
```

Also remember rendering depends on the terminal's configured font — a missing glyph in one terminal app may be a font issue, not a config issue.
