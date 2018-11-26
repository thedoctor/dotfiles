PATH=$PATH:$HOME/bin:/usr/sbin:$HOME/.util/bin
export PATH

if [[ $machine = "Mac" ]]; then
    if [ -f "${HOME}/.bashrc-mac" ]; then
        source "${HOME}/.bashrc-mac"
    fi
elif [[ $machine = "Linux" ]]; then
    if [ -f "${HOME}/.bashrc-linux" ]; then
        source "${HOME}/.bashrc-linux"
    fi
fi

export EDITOR="emacsclient -t"

export RBENV_ROOT="${HOME}/.rbenv";
if [[ -d "${RBENV_ROOT}" ]]; then
    export PATH="${RBENV_ROOT}/bin:${PATH}";
    eval "$(rbenv init -)";
fi

# Ergodox
updox(){
    cd "${HOME}/dev/ergodox/tmk_keyboard/keyboard/ergodox"
    make -f Makefile.lufa clean && make -f Makefile.lufa
    cd -
}

web(){
    # TODO: check for scheme and make a smart guess to prepend one if missing.
    python -c "from webbrowser import open; open('${1}')"
}

ngrepl(){
    echo "> sudo ngrep -W byline -d lo port $1"
    sudo ngrep -W byline -d lo port $1
}

mfind(){
    find "$1" -type f -exec grep -iIHn "$2" {} \;
}

mrepl(){
    if [[ $# -eq 3 ]]; then
        read -p "Are you sure you want to replace all occurrences of $2 with $3 in $1? [y/N] " -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            find "$1" -type f -exec sed -i.deleteme "s/$2/$3/g" {} \;
            find "$1" -type f -name "*.deleteme" -exec rm -f {} \;
        else
            echo 'Okay, nvm'
        fi
    else
        echo 'Usage: mrepl DIR search-string replace-string'
    fi
}

dec(){
    find ~/.encrypted/ -type f -iregex ".*${1}.*[gpgasc][gpgasc][gpgasc]" -exec gpg -d {} \;
}

decc(){
    dec $@ | pbcopy
}

viscosity(){
    osascript -e 'tell application "Viscosity" to connect "${1}"'
}

be(){
    bundle exec "$@"
}

# Kubernetes shortcuts

k(){
    echo "> kubectl $@"
    kubectl "$@"
}

kc(){
    if [[ $# -lt 1 ]]; then
        echo "Usage: kc [CONTEXT-TO-SELECT || kubectl-config-ARGS...]"
        k config view
    elif [[ $# -eq 1 ]]; then
        k config use-context $1
    else
        k config $@
    fi
}

kn(){
    if [[ $# -lt 1 ]]; then
        echo "Usage: kn [NAMESPACE-TO-SET || (NAMESPACE|all) KUBECTL-COMMAND-TO-RUN]"
        k get namespaces
    elif [[ $# -eq 1 ]]; then
        k config set-context $(kubectl config current-context) --namespace="$1"
    elif [[ $1 = "all" ]]; then
        k "${@: 2:$#-1}" --all-namespaces
    else
        k --namespace="$1"  "${@: 2:$#-1}"
    fi
}

kl(){
    if [[ $# -lt 1 ]]; then
        echo "Usage: kl [-f] POD-SEARCH-STRING"
        return 1
    fi
    if [[ $1 = "-f" ]]; then
        k logs -f $(kg pods | cut -f 1 -d ' ' |grep $2)
    else
        k logs $(kg pods | cut -f 1 -d ' ' |grep $1)
    fi
}

kssh(){
    echo "Usage: kssh POD-SEARCH-STRING"
    k exec -it $(kg pods | cut -f 1 -d ' ' |grep $1)  -- /bin/bash
}

# Deletes things of [THING-TYPE] fuzzy-matching [SEARCH-STRING]
kd(){
    echo $#
    if [[ $# -lt 1 ]]; then
        echo "Usage: kd [-f] [THING-TYPE] SEARCH-STRING"
        return 1
    fi
    if [[ $# -lt 2 ]]; then
        kdd $@
    elif [[ $# -eq 2 ]] && [[ $1 = '-f' ]]; then
        kdd $@
    elif [[ $1 = "-f" ]]; then
        k delete $2 $(kg $2 | cut -f 1 -d ' ' |grep $3) --grace-period=0
    else
        k delete $1 $(kg $1 | cut -f 1 -d ' ' |grep $2)
    fi
}

# Deletes all things with exact name match
kdd(){
    if [[ $1 = '-f' ]]; then
        THING="${2} --grace-period=0"
    else
        THING=$1
    fi
    k delete deployment $THING 2>/dev/null
    k delete rc $THING 2>/dev/null
    k delete service $THING 2>/dev/null
    k delete pod $THING 2>/dev/null
}

kdsc(){
    if [[ $# -lt 1 ]]; then
        echo "Usage: kdsc THING-TYPE SEARCH-STRING"
    else
        k describe $1 $(kg $1 | cut -f 1 -d ' ' |grep $2)
    fi
}

# Execute a command in a pod
ke(){
    if [[ $# -lt 2 ]]; then
        echo "Usage: ke POD-SEARCH-STRING COMMAND"
    else
        k exec $(k get pods | cut -f 1 -d ' ' |grep $1)  "${@: 2:$#-1}"
    fi
}

kg(){
    if [[ $# -lt 1 ]]; then
        printf "Usage: kg [THING-TYPE(pods|rc|rs|deployments|svc|pv|pvc|...)]\n"
        printf "\n--namespaces--\n"
        kg namespaces
        printf "\n--persistentvolumes--\n"
        kg pv
        printf "\n--persistentvolumeclaims--\n"
        kg pvc
        printf "\n--services--\n"
        kg svc
        printf "\n--deployments--\n"
        kg deployments
        printf "\n--replicationcontrollers--\n"
        kg rc
        printf "\n--pods--\n"
        kg pods
    else
        k get "${@}"
    fi
}

# Deletes a pod
kpd(){
    if [[ $# -lt 1 ]]; then
        echo "Usage: kpd POD-IDENTIFIER-SUBSTRING"
    else
        k delete pod
    fi
}

jiranum(){
    prefix=""
    env_re="[0-9]+"
    jira_re="[^/]*/?([0-9]{3,4}).*"
    if [[ $(sexy_bash_prompt_get_git_branch) =~ $jira_re ]]; then
        prefix="${BASH_REMATCH[1]}";
    else
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
    if [[ $time == 'n' || $time == 'N' || $time == '' ]]; then
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
    if [[ $# -eq 0 ]]; then
        echo "Usage: grel TAGNAME [BRANCH]"
    else
        read -p "Did you git changelog? [Y/n]" -e yn
        if [[ $yn == 'n' || $yn == 'N' ]]; then
            return 1
        fi

        g tag -d "$1" &>/dev/null
        g p origin --tags :"$1" &>/dev/null
        g t -a "$1" -m "Release $1" && g p origin --tags
    fi

    if [[ $# -eq 2 ]]; then
        g p origin "$2"
    fi
}

irclog (){
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
    if [[ $# -eq 0 ]]; then
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

glp(){
    if [[ $# -eq 0 ]]; then
        g log -p --stat
    else
        g log -p -$1 --stat
    fi
}

up(){
    local d=""
    limit=$1
    for ((i=1 ; i <= limit ; i++)); do
        d=$d/..
    done
    d=$(echo $d | sed 's/^\///')
    if [[ -z "$d" ]]; then
        d=..
    fi

    if [[ $# -gt 1 ]]; then
        cd $d; cd $2
    else
        cd $d
    fi
}

pc(){
    if [[ $# -eq 0 ]]; then
        echo "Usage: pc password-name-fragment [result-set-index]"
    else
        local lst=( $(find ${HOME}/.password-store -name "*${1}*" | cut -d'/' -f 5-| sed 's/.gpg$//') )

        if [[ ${#lst[@]} -gt 1 ]]; then
            if [[ $# -gt 1 ]]; then
                pass -c ${lst[$(($2 - 1))]}
            else
                for i in "${!lst[@]}"; do
                    printf "%s\t%s\n" "$(($i+1))" "${lst[$i]}"
                done
            fi
        else
            pass -c ${lst[0]}
        fi
    fi
}

alias bashrc='source ~/.bashrc'

alias cp='rsync -aP'

alias dl="tail -f /tmp/drupal_debug.txt"
alias dockerc="docker-compose"
alias dockerm="docker-machine"

alias e='ec'
alias eamcs='emacs'
alias emac='emacs'
alias emcas='emacs'
alias emasc='emacs'

alias erc="e ${HOME}/.bashrc && source ${HOME}/.bashrc"
alias edox="e ${HOME}/dev/ergodox/tmk_keyboard/keyboard/ergodox/keymap.c"
alias egw="emacsclient -t ${HOME}/.gemwallet"

alias flag='toilet -f mono12 '
alias ll='exa -l'
alias la='exa -la'
alias l='la'

alias ore="pass -c gem/oregon; osascript -e 'tell application \"Viscosity\" to connect \"gem-oregon\"'"
alias kvpn="pass -c gem/kube-vpn; osascript -e 'tell application \"Viscosity\" to connect \"gem-kube\"'"

alias rmpyc="find . -type f -name '*.pyc' -exec rm -f {} \;"
alias rmtilda="find . -name '*~' -exec rm {} \;"
alias rmhash="find . -name '*\#*' -exec rm {} \;"
alias cltr="rmpyc && rmtilda && rmhash"
alias nifty="sudo kextunload /System/Library/Extensions/AppleStorageDrivers.kext/Contents/PlugIns/AppleUSBCardReader.kext && sudo kextload /System/Library/Extensions/AppleStorageDrivers.kext/Contents/PlugIns/AppleUSBCardReader.kext"

alias wiki="web https://gemology.atlassian.net/wiki/display/GE/Gem+Engineering"
alias kraken="pc kraken && web https://kraken.com/login"
alias naw="web https://www.youtube.com/watch?v=-K7fCQlUhj0"
note(){
    e "~/notes/$(date +%Y-%m-%d)-$1.md"
}

# Spring stuff
alias spring="springctl "

# Run twolfson/sexy-bash-prompt
if [[ -f ~/.bash_prompt && -n $SEXY ]]; then
    . ~/.bash_prompt
fi

if [[ -f ~/.git-completion.bash ]]; then
    source ~/.git-completion.bash
    __git_complete g __git_main
    __git_complete gc _git_commit
    __git_complete gl _git_log
    __git_complete gchk _git_checkout
    __git_complete gm _git_merge
    __git_complete pl _git_pull
    __git_complete p _git_push
fi

# Python Environment

export PYTHONSTARTUP=~/.pythonrc
export WORKON_HOME=~/Envs
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# pyenv install 3.6.6
# pyenv global 3.6.6

# Basic Shell
if [[ "$SEXY" != "1" ]]; then
    export PS1="$(pwd): "
fi

### Cargo is the rust package manager
export PATH="${HOME}/.cargo/bin:$PATH"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:${HOME}/.local/bin:$PATH"

# THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
# ^--- well that's bullshit
export SDKMAN_DIR="${HOME}/.sdkman"
[[ -s "${HOME}/.sdkman/bin/sdkman-init.sh" ]] && source "${HOME}/.sdkman/bin/sdkman-init.sh"

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
