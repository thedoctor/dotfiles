PATH=$PATH:$HOME/bin:/usr/sbin:$HOME/.util/bin
export PATH

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export LS_COLORS=gxBxhxDxfxhxhxhxhxcxcx

export EDITOR="emacsclient -t"

fateproc(){
  sudo ps aux| grep server | grep -v grep | awk '{ print $2 }' | xargs kill
  $@
}

alias erc="e ${HOME}/.bashrc && source ${HOME}/.bashrc"
alias edox="e ${HOME}/dev/ergodox/tmk_keyboard/keyboard/ergodox/keymap.c"
alias egw="emacsclient -t ${HOME}/.gemwallet"


updox(){
  cd "${HOME}/dev/ergodox/tmk_keyboard/keyboard/ergodox"
  make -f Makefile.lufa clean && make -f Makefile.lufa
  cd -

  # cd "${HOME}/dev/ergodox"
  # python3 'build-scripts/gen-ui-info.py' --current-date '2014-06-09 22:35:15+02:00' --git-commit-date '2012-12-20 16:34:08 -0800' --git-commit-id '43ee200b2b6e2e234ee8d13d8824e1d5068ba7d0' --map-file-path 'tmk_keyboard/keyboard/ergodox_lufa.map' --source-code-path 'tmk_keyboard/keyboard' --matrix-file-path 'tmk_keyboard/common/matrix.h' --layout-file-path 'tmk_keyboard/keyboard/ergodox/keymap.c' > ui-info
  # python3 build-scripts/gen-layout.py --ui-info-file ui-info >layout.html
  # python -c 'from webbrowser import open_new; open_new("./layout.html");'
  # cd -
}

gemsave(){
  if [ -f "${HOME}/.gemwallet" ]; then
      cp "${HOME}/.gemwallet" "${HOME}/.gemwallet.${1}"
  fi
}

gemset(){
  if [ -f "${HOME}/.gemwallet" ]; then
     rm "${HOME}/.gemwallet"
  fi
  cp "${HOME}/.gemwallet.${1}" "${HOME}/.gemwallet"
}

jiraprefix(){
  prefix=""
  env_re="[0-9]+"
  jira_re="[^/]*/?([0-9]{3,4}).*"
  if [[ $(__git_ps1) =~ $jira_re ]]; then prefix="GP-${BASH_REMATCH[1]}"; else
      if [[ "${JIRA_NUM}" =~ $env_re ]]; then
          prefix="GP-${JIRA_NUM}"
      else
          prefix="";
      fi
  fi
  echo "$prefix "
}

ngrepl() {
  echo "> sudo ngrep -W byline -d lo port $1"
  sudo ngrep -W byline -d lo port $1
}

worker() {
  sudo ssh -i ~/.ssh/twogem-worker ubuntu@$1
}

mfind(){
  find "$1" -type f -exec grep -iIHn "$2" {} \;
}
mrepl(){
  if [[ $# -eq 3 ]]; then
      read -p "Are you sure you want to replace all occurrences of $2 with $3 in $1? [Y/n] " -r
      echo
      if [[ $REPLY =~ ^[Yy]$ ]]
      then
          find "$1" -type f -exec sed -i "s/$2/$3/g" {} \;
      fi
  else
      echo 'Usage: mrepl DIR search-string replace-string'
  fi
}

decc(){
  dec $@ | pbcopy
}
dec(){
  find ~/encrypted/ -type f -iregex ".*${1}.*[gpgasc][gpgasc][gpgasc]" -exec gpg -d {} \;
}

be(){
  bundle exec "$@"
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

gm(){
  g merge "$@"
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

grel(){
  if [ $# -eq 0 ]; then
      echo "Usage: grel TAGNAME [BRANCH]"
  else
      g tag -d "$1" &>/dev/null ; psho --tags :"$1" &>/dev/null ; g tag -a "$1" -m "Release $1" && psho --tags
  fi

  if [ $# -eq 2 ]; then
      psho "$2"
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
  g commit -a "$@"
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

gs(){
  echo '> git status'
  git status
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

alias ll='ls -la'
alias cp='rsync -aP'
# I'm bad at typing
alias e='emacsclient -t'
alias eamcs='emacs'
alias emac='emacs'
alias emcas='emacs'
alias emasc='emacs'
alias flag='toilet -f mono12 '

alias cc="drush rr && drush cc all"
alias ccfg="drush rr && drush cc all && fg"
alias dl="tail -f /tmp/drupal_debug.txt"
alias rmpyc="find . -type f -name '*.pyc' -exec rm -f {} \;"
alias rmtilda="find . -name '*~' -exec rm {} \;"
alias cltr="rmpyc; rmtilda"
alias remount="sudo kextunload /System/Library/Extensions/AppleStorageDrivers.kext/Contents/PlugIns/AppleUSBCardReader.kext && sudo kextload /System/Library/Extensions/AppleStorageDrivers.kext/Contents/PlugIns/AppleUSBCardReader.kext"
alias clearberks='rm -rf ~/.berkshelf/vagrant-berkshelf/shelves/*'
alias roundpy='gemp && cd roundpy'
alias bashrc='source ~/.bashrc'

export PYTHONSTARTUP=~/.pythonrc

export RBENV_ROOT="${HOME}/.rbenv"; if [ -d "${RBENV_ROOT}" ]; then export PATH="${RBENV_ROOT}/bin:${PATH}"; eval "$(rbenv init -)"; fi

source ~/.git-prompt.sh

if [ -f ~/.git-completion.bash ]; then
    source ~/.git-completion.bash
    __git_complete g __git_main
    __git_complete gc _git_commit
    __git_complete gchk _git_checkout
    __git_complete gm _git_merge
    __git_complete pll _git_pull
    __git_complete psh _git_push
fi


if [ ! -e "${HOME}/.local/bin/virtualenvwrapper.sh" ]; then
    if [ ! -e "${HOME}/virtualenvwrapper.sh" ]; then
        if [ ! -e "/usr/local/bin/virtualenvwrapper.sh" ]; then
            pip install --user virtualenv
            pip install --user virtualenvwrapper
        fi
    fi
fi

export WORKON_HOME=~/Envs

if [ -f "/usr/local/bin/virtualenvwrapper.sh" ]; then
    source /usr/local/bin/virtualenvwrapper.sh
    workon py
fi


if [ -f "${HOME}/.local/bin/virtualenvwrapper.sh" ]; then
    source ~/.local/bin/virtualenvwrapper.sh
    export PATH="$PATH:${HOME}/.local/bin"
    workon py
fi

if [ -f "${HOME}/virtualenvwrapper.sh" ]; then
    source ~/virtualenvwrapper.sh
    workon py
fi

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:${HOME}/.local/bin:$PATH"

### For tx, bx, and ku (pycoin cli tools)
PYCOIN_CACHE_DIR=~/.pycoin_cache
PYCOIN_SERVICE_PROVIDERS=BLOCKR_IO:BLOCKCHAIN_INFO:BITEASY:BLOCKEXPLORER
export PYCOIN_CACHE_DIR PYCOIN_SERVICE_PROVIDERS

# Run twolfson/sexy-bash-prompt
. ~/.bash_prompt
