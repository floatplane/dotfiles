# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

function source_if_exists {
  if [[ -s $1 ]] ; then
      source $1
  # elif [[ "$PS1" ]]; then
  #     # Warn when running interactively, not otherwise
  #     echo $1 not found.
  fi
}

function command_exists_on_path () {
  type "$1" &> /dev/null ;
}

# Tweak the path.
export PATH=/opt/local/bin:/opt/local/sbin:~/bin:~/src/hadoop-0.20.2/bin:$PATH

if [[ $(uname) =~ "Darwin" ]]; then
    # OS X limits us to 256 open file descriptors by default. That's kinda
    # small. Node can chew that up easily.
    # See http://docs.basho.com/riak/latest/ops/tuning/open-files-limit/#Mac-OS-X for
    # information about creating /Library/LaunchDaemons/limit.{maxfiles,maxprocs}.plist
    ulimit -n 10000

    # Move /usr/local/bin to the front for Homebrew
    export PATH=/usr/local/bin:$PATH
fi

# If not running interactively, don't do any of these things
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


  # if [[ -x `command -v atom` ]]; then
  #   alias ec='atom --foreground'
  #   export EDITOR="atom --wait"
  # elif [[ -d "/Applications/Emacs.app/Contents/MacOS/bin" ]]; then
  if [[ -d "/Applications/Emacs.app/Contents/MacOS/bin" ]]; then
    alias emacs='open -a emacs'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    alias ec='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -a /Applications/Emacs.app/Contents/MacOS/Emacs -n'
    alias edit="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -a /Applications/Emacs.app/Contents/MacOS/Emacs -t"
    export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -a /Applications/Emacs.app/Contents/MacOS/Emacs -t"
  else
    alias ec="emacsclient --alternate-editor= -n"
    alias e="emacsclient --alternate-editor= -n"
    alias edit="emacsclient --alternate-editor= -t"
    export EDITOR="emacsclient --alternate-editor= -t"
  fi

  alias ls="ls -Gp"
  alias l="ls -la"
  alias p="pushd ~/src/Paper"
  function up {
    dir="$(up_dir $@)"
    pushd $dir
  }
  if [[ $(uname) =~ "Darwin" ]]; then
    alias xc="open -a Xcode"
  else
    alias pbcopy="xclip -i -sel clip"
    alias pbpaste="xclip -o"
    alias ack="ack-grep"
  fi
  alias ag="ag --pager='less -FXR' --depth 100"

  export GIT_EDITOR=$EDITOR
  export VISUAL=$EDITOR
  # PYTHONPATH=$PYTHONPATH:~/python/boto-1.8d

  PROMPT_HOSTNAME=`hostname -s`
  # if [ -x /usr/bin/ec2metadata ]; then
  #     INSTANCE_ID=`ec2metadata --instance-id`
  #     INSTANCE_IP=`ec2metadata --public-ipv4`
  #     PROMPT_HOSTNAME="$INSTANCE_ID ($INSTANCE_IP)"
  # fi

  PS1='\u@$PROMPT_HOSTNAME:\w\$ '

  # dummy functions so that the prompt doesn't break if the
  # autocompletion files aren't found
  function __git_ps1 {
    echo -n ''
  }

  function __git_status_flag {
    echo -n ''
  }

  # Alias git to the hub command
  if command_exists_on_path hub; then
      eval "$(hub alias -s)"
  fi

  # Load hub tab-completion
  source_if_exists ~/dotfiles/hub.bash_completion.sh

  # tab-completion for macports
  source_if_exists /opt/local/etc/bash_completion

  # tab-completion for homebrew
  if command_exists_on_path brew; then
      source_if_exists `brew --prefix`/etc/bash_completion
  fi

  # tab-completion for rake and cap
  source_if_exists ~/bin/rake_cap_bash_autocomplete.sh

  # tab-completion for git subcommands
  source_if_exists ~/dotfiles/git-completion.bash
  source_if_exists ~/dotfiles/git-prompt.sh

  source_if_exists ~/dotfiles/ssh_completion.sh

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
  PS1='\u@\[\e[1m\]$PROMPT_HOSTNAME\[\e[22m\]: \[\e[0m\]$(__truncated_current_directory)\[\e[22;35m\]$(__git_ps1 " [\[\e[33m\]$(__git_status_flag)\[\e[35m\]%s]")\[\e[33m\] \$ \[\e[0m\]'

  # devtunnel hostname
  export DEVTUNNEL_HOST=`whoami`-`hostname -s`

  if [[ -d "/Applications/Postgres.app/Contents/MacOS/bin" ]]; then
      PATH=/Applications/Postgres.app/Contents/MacOS/bin:$PATH
  fi
fi

# Add depot tools to the path if they're there
[[ -s "$HOME/depot_tools" ]] && PATH=$PATH:$HOME/depot_tools

# Load local script, if any
source_if_exists ~/.bashrc.local

# [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.

# PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# Get grunt tab completion if it's installed
command -v "grunt" > /dev/null && eval "$(grunt --completion=bash)"

export NVM_DIR="/Users/brian/.nvm"
[[ -s "$NVM_DIR/nvm.sh" ]] && . "$NVM_DIR/nvm.sh"  # This loads nvm
source "$HOME/.cargo/env"
