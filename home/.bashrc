ngrepl() {
  echo "> sudo ngrep -W byline -d lo port $1"
  sudo ngrep -W byline -d lo port $1
}

mfind(){
  find "$1" -type f -exec grep -iIHn "$2" {} \;
}

g(){
  echo "> git $@"
  git "$@"
}

ga(){
  g add "$@"
}

gr(){
  g reset HEAD "$@"
}

psh(){
  g push "$@"
}

psho(){
  if [ $# -eq 0 ]
  then
    psh origin HEAD
  else
    psh origin "$@"
  fi
}

pll(){
  g pull "$@"
}

pllo(){
  if [ $# -eq 0 ]
  then
    pll origin HEAD
  else
    pll origin "$@"
  fi
}

gca(){
  g commit -am "$1"
}

gb(){
  g branch "$@"
}

gc(){
  if [ $# -eq 0 ]
  then
    g commit
  else
    g commit "${@: 1:$#-1}" -m "${@: -1}"
  fi
}

gd(){
  g diff "$@"
}

gchk(){
  g checkout "$@"
}

gl(){
  g log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit
}

glp() {
  if [ $# -eq 0 ]
  then
    g log -p --stat
  else
    g log -p -$1 --stat
  fi
}


alias gs="echo '> git status' && git status"

up(){
  local d=""
  limit=$1
  for ((i=1 ; i <= limit ; i++))
    do
      d=$d/..
    done
  d=$(echo $d | sed 's/^\///')
  if [ -z "$d" ]; then
    d=..
  fi
  cd $d
}


source ~/.git-prompt.sh

RED="\[\033[0;31m\]"
YELLOW="\[\033[0;33m\]"
GREEN="\[\033[0;32m\]"

PS1="$RED\$(date +%H:%M) \w$YELLOW\$(__git_ps1)$GREEN\$ "
PATH=$PATH:$HOME/bin:/usr/sbin
export PATH

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

alias ll='ls -la'
alias cp='rsync -aP'
alias eamcs='emacs'
alias emasc='emacs'
alias flag='toilet -f mono12 '

alias cc="drush rr && drush cc all"
alias ccfg="drush rr && drush cc all && fg"
alias dl="tail -f /tmp/drupal_debug.txt"
alias rmpyc="find . -type f -name '*.pyc' -exec rm {} \;"
alias rmtilda="find . -name '*~' -exec rm {} \;"
alias remount="sudo kextunload /System/Library/Extensions/AppleStorageDrivers.kext/Contents/PlugIns/AppleUSBCardReader.kext && sudo kextload /System/Library/Extensions/AppleStorageDrivers.kext/Contents/PlugIns/AppleUSBCardReader.kext"

export PYTHONSTARTUP=~/.pythonrc
export RBENV_ROOT="${HOME}/.rbenv"; if [ -d "${RBENV_ROOT}" ]; then export PATH="${RBENV_ROOT}/bin:${PATH}"; eval "$(rbenv init -)"; fi
export WORKON_HOME=~/Envs
if [ ! -e "/usr/local/bin/virtualenvwrapper.sh" ]
then
    pip install virtualenv
    pip install virtualenvwrapper
fi

source virtualenvwrapper.sh
### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
source ~/.git-completion.bash
alias clearberks='rm -rf ~/.berkshelf/vagrant-berkshelf/shelves/*'
workon py
