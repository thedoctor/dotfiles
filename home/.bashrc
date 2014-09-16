mfind(){
    find "$1" -type f -exec grep -iIHn "$2" {} \;
}

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
alias flag='toilet -f mono12 '

alias cc="drush rr && drush cc all"
alias ccfg="drush rr && drush cc all && fg"
alias dl="tail -f /tmp/drupal_debug.txt"

export PYTHONSTARTUP=~/.pythonrc
