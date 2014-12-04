ngrepl() {
  echo "> sudo ngrep -W byline -d lo port $1"
  sudo ngrep -W byline -d lo port $1
}

mfind(){
  find "$1" -type f -exec grep -iIHn "$2" {} \;
}

push(){
  echo "> git push $@"
  git push "$@"
}

pusho(){
  echo "> git push origin $@"
  push origin "$@"
}

pull(){
  echo "> git pull $@"
  git pull "$@"
}

pullo(){
  echo "> git pull origin $@"
  pull origin "$@"
}

gca(){
  echo "> git commit -am \"$1\""
  git commit -am "$1"
}

gc(){
  echo "> git commit ${@: 1:$#-1} -m \"${@: -1}\""
  git commit "${@: 1:$#-1}" -m "${@: -1}"
}

gd(){
  echo "> git diff $@"
  git diff "$@"
}

gchk(){
  echo "> git checkout $@"
  git checkout "$@"
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

gitlog(){
    git log -$1 --oneline --stat
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


export PYTHONSTARTUP=~/.pythonrc
export RBENV_ROOT="${HOME}/.rbenv"; if [ -d "${RBENV_ROOT}" ]; then export PATH="${RBENV_ROOT}/bin:${PATH}"; eval "$(rbenv init -)"; fi
export WORKON_HOME=~/Envs
if [ ! -e "/usr/local/bin/virtualenvwrapper.sh" ]
then
    pip install virtualenv
    pip install virtualenvwrapper
fi

source virtualenvwrapper.sh
