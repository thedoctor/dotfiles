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
        read -p "Are you sure you want to replace all occurrences of $2 with $3 in $1? [Y/n] " -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            find "$1" -type f -exec sed -i "s/$2/$3/g" {} \;
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

k(){
    kubectl "$@"
}

knamespace(){
    kubectl config set-context minikube --namespace="$1"
}

kn(){
    kubectl --namespace "$1" "${@: 2:$#-1}"
}

kl(){
    ${HOME}/dev/gem/projects/gem-os-minikube/bin/gkube-podLog.sh $1
}

kd(){
    k delete deployment $1
    k delete service $1
    if [[ $1 = 'gem-gateway' ]]; then
        ${HOME}/dev/gem/projects/gem-os-minikube/bin/registerContainers.sh $1 --exclude gradle
        ${HOME}/dev/gem/projects/gem-os-minikube/bin/gkube-deployService.sh $(k config view -o jsonpath='{.contexts[?(@.name == "minikube")].context.namespace}') $1 --expose NodePort
    else
        ${HOME}/dev/gem/projects/gem-os-minikube/bin/registerContainers.sh $1
        ${HOME}/dev/gem/projects/gem-os-minikube/bin/gkube-deployService.sh $(k config view -o jsonpath='{.contexts[?(@.name == "minikube")].context.namespace}') $1
    fi
}

ke(){
    if [[ $# -lt 1 ]]; then
        echo "Usage: kp POD-IDENTIFIER-SUBSTRING"
    else
        k exec $(k get pods | cut -f 1 -d ' ' |grep $1)  "${@: 2:$#-1}"
    fi
}

kpd(){
    if [[ $# -lt 1 ]]; then
        echo "Usage: kpd POD-IDENTIFIER-SUBSTRING"
    else
        k delete pod $(k get pods | cut -f 1 -d ' ' |grep $1)
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

alias e='emacsclient -t'
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
