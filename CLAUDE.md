# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles for macOS Rails developer using zsh/Emacs, managed with GNU Stow.

## Commands

```bash
# Install all dependencies
cd brew && brew bundle

# Symlink packages to home directory
stow -t ~ -v --dotfiles <packages>

# Unlink a package
stow -t ~ -D <package>

# Dry run (preview changes)
stow -t ~ -v -n --dotfiles <package>
```

## Architecture

### Stow Package Structure

Each top-level directory is a stow package. Files with `dot-` prefix become dotfiles (e.g., `dot-zshrc` becomes `.zshrc`).

| Package | Description |
|---------|-------------|
| brew | Homebrew Brewfile |
| claude | Claude Code config (agents, rules, commands) |
| emacs | Emacs configuration |
| gh | GitHub CLI config |
| git | Git config and ignore |
| karabiner | Karabiner-Elements key remapping |
| raycast | Raycast launcher config |
| readline | Readline inputrc |
| ruby | Ruby/rbenv configuration |
| terminal | Terminal.app settings |
| tmux | Tmux configuration |
| zsh | Zsh configuration |

### Claude Code Configuration

The `claude/dot-claude/` directory contains:
- `agents/` - Custom agent definitions
- `rules/` - Coding standards and workflows
- `commands/` - Slash commands (skills)

These get symlinked to `~/.claude/` for global use across all projects.
