# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

function source_if_exists {
  if [[ -s $1 ]] ; then
      source $1
  elif [[ "$PS1" ]]; then
      # Warn when running interactively, not otherwise
      echo $1 not found.
  fi
}

# If not running interactively, don't do anything
if [[ -n "$PS1" ]]; then

  # don't put duplicate lines in the history. See bash(1) for more options
  # ... or force ignoredups and ignorespace
  HISTCONTROL=ignoredups:ignorespace

  # append to the history file, don't overwrite it
  shopt -s histappend

  # for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
  HISTSIZE=1000
  HISTFILESIZE=2000

  # check the window size after each command and, if necessary,
  # update the values of LINES and COLUMNS.
  shopt -s checkwinsize

  # make less more friendly for non-text input files, see lesspipe(1)
  [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

  # set variable identifying the chroot you work in (used in the prompt below)
  if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
  fi

  # set a fancy prompt (non-color, unless we know we "want" color)
  case "$TERM" in
    xterm-color) color_prompt=yes;;
  esac

  # uncomment for a colored prompt, if the terminal has the capability; turned
  # off by default to not distract the user: the focus in a terminal window
  # should be on the output of commands, not on the prompt
  #force_color_prompt=yes

  if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
          # We have color support; assume it's compliant with Ecma-48
          # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
          # a case would tend to support setf rather than setaf.)
      color_prompt=yes
    else
      color_prompt=
    fi
  fi

  if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
  else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
  fi
  unset color_prompt force_color_prompt

  # If this is an xterm set the title to user@host:dir
  case "$TERM" in
    xterm*|rxvt*)
      PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
      ;;
    *)
      ;;
  esac

  # enable color support of ls and also add handy aliases
  if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
      # WARNING: enabling this can cause multi-second delays due to NFS latency
      #alias ls='ls --color=auto'
      #alias dir='dir --color=auto'
      #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
  fi

  # some more ls aliases
  alias ll='ls -alF'
  alias la='ls -A'
  alias l='ls -CF'

  # Alias definitions.
  # You may want to put all your additions into a separate file like
  # ~/.bash_aliases, instead of adding them here directly.
  # See /usr/share/doc/bash-doc/examples in the bash-doc package.

  if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
  fi

  # enable programmable completion features (you don't need to enable
  # this, if it's already enabled in /etc/bash.bashrc and /etc/profile
  # sources /etc/bash.bashrc).
  if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
  fi

  if [[ -d "/Applications/Emacs.app/Contents/MacOS/bin" ]]; then
    alias emacs='open -a emacs'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    alias ec='emacsclient -a /Applications/Emacs.app/Contents/MacOS/Emacs'
    alias vi="emacsclient -a /Applications/Emacs.app/Contents/MacOS/Emacs -t"
    export EDITOR="emacsclient -a /Applications/Emacs.app/Contents/MacOS/Emacs -t"
  else
    alias ec="emacsclient --alternate-editor= -n"
    alias e="emacsclient --alternate-editor= -n"
    alias vi="emacsclient --alternate-editor= -t"
    export EDITOR="emacsclient --alternate-editor= -t"
  fi

  alias ls="ls -Gp"
  alias l="ls -la"
  # alias mysql=/usr/local/mysql/bin/mysql
  # alias mysqladmin=/usr/local/mysql/bin/mysqladmin
  alias start=~/bin/start_everything.rb
  alias src="pushd ~/src"
  alias sg="pushd ~/src/socialgold/trunk"
  alias j="pushd ~/src/jambool/trunk/"
  alias jd="pushd ~/src/jambool/trunk/data"
  alias inv="pushd ~/src/jambool/trunk/data/inventory"
  alias ops="pushd ~/src/svn/ops/trunk"
  alias api="pushd ~/src/api"
  function up {
    dir="$(up_dir $@)"
    pushd $dir
  }
  if [[ $(uname) =~ "Darwin" ]]; then
    alias ack="ack --pager=\"less -FXR\""
  else
    alias pbcopy="xclip -i -sel clip"
    alias pbpaste="xclip -o"
    alias ack="ack-grep --pager=\"less -FXR\""
  fi

  export GIT_EDITOR=$EDITOR
  export VISUAL=$EDITOR
  # PYTHONPATH=$PYTHONPATH:~/python/boto-1.8d

  PS1='\u@\h:\w\$ '

  # dummy functions so that the prompt doesn't break if the
  # autocompletion files aren't found
  function __git_ps1 {
    echo -n ''
  }

  function __git_status_flag {
    echo -n ''
  }

  # tab-completion for macports
  source_if_exists /opt/local/etc/bash_completion

  # tab-completion for rake and cap
  source_if_exists ~/bin/rake_cap_bash_autocomplete.sh

  # tab-completion for git subcommands
  source_if_exists /etc/bash_completion.d/git
  source_if_exists /etc/bash_completion.d/g4

  export PATH=/opt/local/bin:/opt/local/sbin:~/bin:~/src/hadoop-0.20.2/bin:$PATH
  # git status with a dirty flag
  function __git_status_flag {
    git_status="$(git status 2> /dev/null)"
    remote_pattern="^# Your branch is (.*) of"
    diverge_pattern="# Your branch and (.*) have diverged"
    if [[ ! ${git_status}} =~ "working directory clean" ]]; then
      # state="\0342\0230\0207"
      state="⚡"
      spacer=" "
    fi

    if [[ ${git_status} =~ ${remote_pattern} ]]; then
      spacer=" "
      if [[ ${BASH_REMATCH[1]} == "ahead" ]]; then
        remote="↑"
      else
        remote="↓"
      fi
    fi

    if [[ ${git_status} =~ ${diverge_pattern} ]]; then
      remote="↕"
      spacer=" "
    fi

    echo -e "${state}${remote}${spacer}"
  }

  function __truncated_current_directory {
    let pwdmaxlen="$(tput cols) - 60"
    let pwdhalfmaxlen="$pwdmaxlen / 2"
    trunc_symbol="..."

    # TODO: set escaped_home_dir to $HOME, then substitute \/ for \
    escaped_home_dir="\/home\/bsharon"
    if [[ $(uname) =~ "Darwin" ]]; then escaped_home_dir="\/Users\/bsharon"; fi

    curr_dir=$(echo ${PWD} | sed -e "s/^${escaped_home_dir}/~/" -e "s/^\/usr\/local\/google\/vc/~\/clients\/vc/")
    if [ ${#curr_dir} -gt $pwdmaxlen ]
    then
     pwdoffset=$(( ${#curr_dir} - $pwdhalfmaxlen ))
      echo "${curr_dir:0:$pwdhalfmaxlen}${trunc_symbol}${curr_dir:$pwdoffset:$pwdhalfmaxlen}"
    else
      echo ${curr_dir}
    fi
  }

  # the prompt itself
  PS1='\u@\[\e[1m\]\h\[\e[22m\]: \[\e[0m\]$(__truncated_current_directory)\[\e[22;35m\]$(__git_ps1 " [\[\e[33m\]$(__git_status_flag)\[\e[35m\]%s]")\[\e[33m\] \$ \[\e[0m\]'

fi

# Load google-specific goodies
source_if_exists ~/.bashrc.google

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
