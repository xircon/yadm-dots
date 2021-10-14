unalias lsd
ttyctl -f
stty icrnl

stty -ixon
stty -ixoff

ttyctl -f

#[ -z "$TMUX"  ] && { tmux attach || exec tmux new-session && exit;}

setopt noflowcontrol

#setopt GLOB_DOTS


#eval "$(ntfy shell-integration)"

OPENER=mimeo
VISUAL=micro
export "VISUAL_EDITOR"=micro
export EDITOR
#export XDG_CONFIG_HOME=/path/to/nowhere
export PAGER=less
export ELSHOME=~/.els
export PATH=$PATH:$ELSHOME/bin:/root/.cargo/bin
export PATH=$PATH:$HOME/.local/bin:/home/xircon/.cargo/bin
export TERM=alacritty
export COLORTERM=truecolor
export XDG_RUNTIME_DIR="/run/user/$UID"
export DBUS_SESSION_BUS_ADDRESS="unix:path=${XDG_RUNTIME_DIR}/bus"


source $HOME/.config/plasma-workspace/env/path.sh

source ~/.local/share/icons-in-terminal/icons_bash.sh

#source /usr/share/zsh/share/antigen.zsh

#antigen bundle mollifier/anyframe

eval "$(zoxide init zsh)"


# fd
export FZF_DEFAULT_COMMAND='fd --type file'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/dircycle
# ctrl+shift+left/right
plugins=(dircycle autojump alias-tips)

autoload -U promptinit; promptinit
autoload -Uz compinit
autoload -U mapfile
zmodload zsh/mapfile    
compinit
compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
zstyle ':completion:*' menu select
zstyle ':completion::complete:*' gain-privileges 1

source /usr/share/zsh/plugins/zsh-autopair/autopair.zsh
autopair-init

#source /usr/bin/goto

source /usr/share/zsh/plugins/alias-tips/alias-tips.plugin.zsh
source $HOME/.oh-my-zsh/custom/plugins/zsh-histdb/sqlite-history.zsh

autoload -Uz add-zsh-hook
#add-zsh-hook precmd histdb-update-outcome

autoload -U url-quote-magic
zle -N self-insert url-quote-magic


source ~/.oh-my-zsh/custom/plugins/zsh-histdb/histdb-interactive.zsh 
bindkey '^r' _histdb-isearch

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/lib/spaceship-prompt/spaceship.zsh
#source ~/.local/bin/tmuxinator.zsh
prompt spaceship

#directory stack
setopt autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'

SAVEHIST=10
HISTFILE=~/.zsh_history
setopt APPENDHISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_ALL_DUPS

ZSH=/usr/share/oh-my-zsh

DISABLE_AUTO_UPDATE="true"

#BROWSER=/usr/bin/firefox

export POWERLINE_HIDE_USER_NAME=1
export POWERLINE_PATH="short"
export MICRO_TRUECOLOR=1
ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache

if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir $ZSH_CACHE_DIR
fi

w3mimg () { w3m -o imgdisplay=/usr/lib/w3m/w3mimgdisplay $1}

source /usr/share/oh-my-zsh/oh-my-zsh.sh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

ALIASFILE=~/.aliasesrc
source $ALIASFILE

. ~/.aliasesrc

function add_alias() {
    if [[ -z $1 || -z $2 || $# -gt 2 ]]; then
        echo usage:
        echo "\t\$$0 ll 'ls -l'"
    else
        echo "alias $1='$2'" >> $ALIASFILE
        echo "alias ADDED to $ALIASFILE"
    fi
    source $ALIASFILE
}
export NVM_DIR="/home/steve/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm


catl () {
    echo "########################################"
    Z=`locate $1 | head -n1`
    echo $Z
    echo "########################################"
    echo .
    cat $Z
}

change () {
    pushd
    bsp
    echo -n "Update Git? "
    local temp
    read temp
    if [[ $temp = "y" || $temp = "Y" ]]; then
        ~/.config/bspwm/gitup.sh
    fi
    popd
}

alias lll="tyls "

function fkill {
    local pid
    pid=$(ps -ef | sed 1d | fzf -e -m -i +s --reverse --tac --margin=4%,1%,1%,2% --inline-info --header="TAB to (un)select. ENTER to kill selected process(es). ESC or CTRL+C to quit." --prompt='Enter string to filter history > ' | awk '{print $2}')

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}

# fh - repeat history
fh() {
    print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf -e +s --tac | sed 's/ *[0-9]* *//' | sed 's/ *[*]* *//')
}

# cdd - directory history
cdd() {
    cd2=$(dirs -v | fzf -e +s --tac | awk '{print $1;}')
    cd -$cd2
    echo $(pwd) >> ~/.dirhistory 
}

# hdb - repeat history
hdb() {
    print -z $( ([ -n "$ZSH_NAME" ] && histdb --limit 10000 || history) | fzf -e +s --tac |  cut -c 42-)
}



ffd() {
    DIR=`find * -maxdepth 0 -type d -print 2> /dev/null | fzf-tmux --reverse` \
        && cd "$DIR"
}

fda() {
    local dir
    dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m --reverse -e +s) && cd "$dir"
}

falias() {
    cat .aliasesrc | fzf -e -1 --reverse
}

alias sxs="pkill -USR1 -x sxhkd"

autoload -U promptinit; promptinit
prompt spaceship
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_PROMPT_DEFAULT_PREFIX=
SPACESHIP_TIME_SHOW=true
SPACESHIP_DIR_PREFIX=$SPACESHIP_CHAR_SYMBOL
SPACESHIP_EXEC_TIME_ELAPSED=10
SPACESHIP_DIR_COLOR=093
#SPACESHIP_TIME_COLOR=red
SPACESHIP_GIT_SHOW=false
SPACESHIP_NODE_SHOW=false
SPACESHIP_TIME_FORMAT="%D{%Y-%m-%d} %B%F{blue}%T"

bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line
bindkey "^[[1;5C" forward-word
#bindkey "^[[1;5D" backward-word

#gnome-shell-extension-tool -e run-or-raise@edvard.cz &> /dev/null

if [ $TILIX_ID  ] || [ $VTE_VERSION  ]; then
            source /etc/profile.d/vte.sh
fi
h() { if [ -z "$*" ]; then history 1; else history 1 | egrep "$@"; fi; }

stty -ixon

source /home/xircon/.config/broot/launcher/bash/br
#killall xcape
#xmodmap ~/.Xmodmap 2>/dev/null
#xcape -e "Hyper_L=space"&!

