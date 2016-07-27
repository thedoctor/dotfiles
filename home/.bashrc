PATH=$PATH:$HOME/bin:/usr/sbin:$HOME/.util/bin
export PATH

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export LS_COLORS=gxBxhxDxfxhxhxhxhxcxcx

export EDITOR="emacsclient -t"

export PYTHONSTARTUP=~/.pythonrc

export RBENV_ROOT="${HOME}/.rbenv"; if [ -d "${RBENV_ROOT}" ]; then export PATH="${RBENV_ROOT}/bin:${PATH}"; eval "$(rbenv init -)"; fi

export WORKON_HOME=~/Envs

bc(){
  /Users/matt/dev/gem/gembox/projects/elements/build/osx/bitcoin-cli $@;
}
btx(){
  /Users/matt/dev/gem/gembox/projects/elements/build/osx/bitcoin-tx $@;
}

ac(){
  /Users/matt/dev/gem/gembox/projects/elements/build/osx/alpha-cli $@;
}
atx(){
  /Users/matt/dev/gem/gembox/projects/elements/build/osx/alpha-tx $@;
}

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

web(){
    # TODO: check for scheme and make a smart guess to prepend one if missing.
    python -c "from webbrowser import open; open('${1}')"
}

addy(){
  if [ $# -eq 0 ]; then
    echo "Usage: addy INT(mg) [time]"
  else
    if [ $# -eq 1 ]; then
      echo '.' | gcalcli --calendar Medical --title "adderall ${1}mg" --when "`date`" --duration 5 --description '' --where '' add
    else
      echo '.' | gcalcli --calendar Medical --title "adderall ${1}mg" --when "${2}" --duration 5 --description '' --where '' add
    fi
  fi
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

ngrepl() {
  echo "> sudo ngrep -W byline -d lo port $1"
  sudo ngrep -W byline -d lo port $1
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
  find ~/.encrypted/ -type f -iregex ".*${1}.*[gpgasc][gpgasc][gpgasc]" -exec gpg -d {} \;
}

viscosity(){
  osascript -e 'tell application "Viscosity" to connect "${1}"'
}

be(){
  bundle exec "$@"
}

jiranum(){
    prefix=""
    env_re="[0-9]+"
    jira_re="[^/]*/?([0-9]{3,4}).*"
    if [[ $(sexy_bash_prompt_get_git_branch) =~ $jira_re ]]; then prefix="${BASH_REMATCH[1]}"; else
        if [[ "${JIRA_NUM}" =~ $env_re ]]; then
            prefix="${JIRA_NUM}"
        else
            prefix="";
        fi
    fi
    echo "$prefix"
}

gcj(){
    read -p "Issue? GEM-[$(jiranum)] " -e num
    [[ $num == '' ]] && num="$(jiranum)" || num="$num"

    read -p "Log time? [N/<value><unit>[ ...]] " -e time
    if [[ $time == 'n' || $time == 'N' || $time == '' ]]
    then
        git commit --author="Matt Smith <matt@gem.co>" -m "GEM-${num} ${@: -1}" "${@: 1:$#-1}"
    else
        git commit --author="Matt Smith <matt@gem.co>" -m "GEM-${num} #time ${time} ${@: -1}" "${@: 1:$#-1}"
    fi
}

g(){
  echo "> git $@"
  git "$@"
}

ga(){
  g a "$@"
}

gt(){
  g t "$@"
}

grs(){
  g rs HEAD "$@"
}

gm(){
  g m "$@"
}

p(){
  g p "$@"
}

grel(){
  if [ $# -eq 0 ]; then
      echo "Usage: grel TAGNAME [BRANCH]"
  else
      read -p "Did you git changelog? [Y/n]" -e yn
      if [[ $yn == 'n' || $yn == 'N' ]]
      then
        return 1
      fi

      g tag -d "$1" &>/dev/null ; g pot :"$1" &>/dev/null ; g t -a "$1" -m "Release $1" && g pot
  fi

  if [ $# -eq 2 ]; then
      g po "$2"
  fi
}

irclog () {
  cat "$@" | grep -v "has joined #" | grep -v "has quit \["  | grep -v "now known as"
}

pl(){
  g pull "$@"
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

gs(){
  g s
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

alias pc='ppc '
ppc () {
  echo "Note: doesn't work for subdirs yet =/"
  read -a matches <<<$(find ~/.password-store/ -name "*$1*" -exec basename {} \;)
  if [ ${#matches[@]} -ne 1 ]; then
    printf '%s\n' "${matches[@]}"
  else
    pass -c $(echo ${matches[0]} | cut -d' ' -f1 | rev | cut -d'.' -f2- | rev)
  fi
}

resetdb(){
  if [ $# -ne 1 ]
  then
    echo "Usage: resetdb DBNAME"
  else
    echo "Dropping schema 'public' in ${1}"
    psql -c "drop schema public cascade;" $1
    echo "Creating new schema 'public' in ${1}"
    psql -c "create schema public;" $1
  fi
}

alias bashrc='source ~/.bashrc'

# I'm bad at typing
alias cc="drush rr && drush cc all"
alias ccfg="drush rr && drush cc all && fg"
alias clearberks='rm -rf ~/.berkshelf/vagrant-berkshelf/shelves/*'
alias cltr="rmpyc; rmtilda"
alias cp='rsync -aP'

alias dl="tail -f /tmp/drupal_debug.txt"
alias dockerc="docker-compose"
alias dockerm="docker-machine"

alias e='emacsclient -t'
alias eamcs='emacs'
alias emac='emacs'
alias emcas='emacs'
alias emasc='emacs'

alias erc="e ${HOME}/.bashrc && source ${HOME}/.bashrc"
alias edox="e ${HOME}/dev/ergodox/tmk_keyboard/keyboard/ergodox/keymap.c"
alias egw="emacsclient -t ${HOME}/.gemwallet"

alias flag='toilet -f mono12 '
alias ll='ls -la'
alias ore="pass -c gem/oregon && osascript -e 'tell application \"Viscosity\" to connect \"gem-oregon\"'"

alias rmpyc="find . -type f -name '*.pyc' -exec rm -f {} \;"
alias rmtilda="find . -name '*~' -exec rm {} \;"
alias remount="sudo kextunload /System/Library/Extensions/AppleStorageDrivers.kext/Contents/PlugIns/AppleUSBCardReader.kext && sudo kextload /System/Library/Extensions/AppleStorageDrivers.kext/Contents/PlugIns/AppleUSBCardReader.kext"
alias roundpy='gemp && cd roundpy'

alias wiki="web https://gemology.atlassian.net/wiki/display/GE/Gem+Engineering"
alias kraken="pc kraken && web https://kraken.com/login"
alias naw="web https://www.youtube.com/watch?v=-K7fCQlUhj0"

# Run twolfson/sexy-bash-prompt
if [[ -f ~/.bash_prompt && -n $SEXY ]]; then
    . ~/.bash_prompt
fi

if [ -f ~/.git-completion.bash ]; then
    source ~/.git-completion.bash
    __git_complete g __git_main
    __git_complete gc _git_commit
    __git_complete gl _git_log
    __git_complete gchk _git_checkout
    __git_complete gm _git_merge
    __git_complete pl _git_pull
    __git_complete p _git_push
fi

# Stupid virtualenv checks, fixme.
if [ ! -e "${HOME}/.local/bin/virtualenvwrapper.sh" ] &&
       [ ! -e "${HOME}/virtualenvwrapper.sh" ] &&
       [ ! -e "/usr/local/bin/virtualenvwrapper.sh" ]; then
    pip install --user virtualenv
    pip install --user virtualenvwrapper
fi

if [[ -f "/usr/local/bin/virtualenvwrapper.sh" && -n $SEXY ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
    workon py
fi

if [[ -f "${HOME}/.local/bin/virtualenvwrapper.sh" && -n $SEXY ]]; then
    source ~/.local/bin/virtualenvwrapper.sh
    export PATH="$PATH:${HOME}/.local/bin"
    workon py
fi

if [[ -f "${HOME}/virtualenvwrapper.sh" && -n $SEXY ]]; then
    source ~/virtualenvwrapper.sh
    workon py
fi

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:${HOME}/.local/bin:$PATH"
### Meteor
export PATH="${HOME}/.meteor:$PATH"

### For tx, bx, and ku (pycoin cli tools)
PYCOIN_CACHE_DIR=~/.pycoin_cache
PYCOIN_SERVICE_PROVIDERS=BLOCKR_IO:BLOCKCHAIN_INFO:BITEASY:BLOCKEXPLORER
export PYCOIN_CACHE_DIR PYCOIN_SERVICE_PROVIDERS

# Basic Shell
if [[ "$SEXY" != "1" ]]; then
    export PS1="$(pwd): "
fi
