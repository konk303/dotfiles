# envs
export EDITOR=emacsclient
export VISUAL=emacsclient
export CLICOLOR=1

# rbenv
eval "$(rbenv init - zsh)"

# zmv
autoload -Uz zmv
# use extended glob (#, ~, ^), without aborting
setopt extended_glob
setopt no_nomatch
# autos
setopt auto_cd
setopt auto_list
setopt auto_menu
setopt auto_param_keys
setopt auto_param_slash
setopt auto_resume
# no beep
setopt no_beep
# other setopts
setopt brace_ccl
setopt correct
setopt equals
setopt no_flow_control
setopt no_hup
setopt ignore_eof
setopt interactive_comments
setopt list_types
setopt long_list_jobs
setopt magic_equal_subst
setopt mark_dirs
setopt multios
setopt numeric_glob_sort
setopt print_eightbit
setopt print_exit_value
setopt pushd_ignore_dups
setopt short_loops
setopt prompt_subst
unsetopt promptcr
setopt transient_rprompt
setopt autopushd

# complitions
source "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc"
source "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc"
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
  autoload -Uz compinit
  compinit
fi

# history settings
HISTSIZE=100000
SAVEHIST=100000
setopt append_history
setopt extended_history
setopt share_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_no_store
setopt hist_verify
zstyle ':completion:*:default' menu select=2
zstyle ':completion:*' list-colors ''
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end

# prompt
source "$(brew --prefix)/etc/bash_completion.d/git-prompt.sh"
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWUPSTREAM=auto
autoload colors
colors
function git_color() {
  local git_info="$(__git_ps1 "%s")"
  if [[ $git_info == *"*"* ]] || [[ $git_info == *"+"* ]]; then
    echo "%{${fg[red]}%}"
  elif [[ $git_info == *"%"* ]]; then
    echo "%{${fg[yellow]}%}"
  else
    echo "%{${fg[green]}%}"
  fi
}
PROMPT="%{${fg_bold[cyan]}%}%#%{${reset_color}%} "
PROMPT2="%{${fg[blue]}%}%_> %{${reset_color}%}"
SPROMPT="%{${fg[red]}%}correct: %R -> %r [nyae]? %{${reset_color}%}"
RPROMPT='$(git_color)$(__git_ps1)%{${reset_color}%}[%{${fg_bold[cyan]}%}%~%{${reset_color}%}] %{${fg_bold[magenta]}%}%D %T%{${reset_color}%}'

# aliases
alias ll='ls -lah'
alias cp='cp -v'
alias mv='mv -v'
alias rm='rm -v'
alias r=rails
alias g='git'
alias e='emacsclient'
alias ee='open /Applications/Emacs.app'
alias cdd='cd $(ghq root)/$(ghq list | peco)'
alias doc='docker-compose'
