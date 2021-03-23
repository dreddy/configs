# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
xterm-color)
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    ;;
*)
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    ;;
esac

# Comment in the above and uncomment this below for a color prompt
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
#if [ "$TERM" != "dumb" ]; then
#    eval "`dircolors -b`"
     alias ls='ls -F'
#    #alias dir='ls --color=auto --format=vertical'
#    #alias vdir='ls --color=auto --format=long'
#fi

# some more ls aliases
alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi


d2h () {
	echo "obase=16; $*" | bc
}

h2d () {
echo "ibase=16; $*" | bc
}

d2b () {
echo "obase=2; $*" | bc
}

b2d () {
echo "ibase=2; $*" | bc
}

setup_julia () {
	export PATH=${PATH}:/usr/local/julia/bin
}

setup_proxy () {
	export http_proxy=http://proxy-dmz.intel.com:911
	export https_proxy=http://proxy-dmz.intel.com:912
	export no_proxy="localhost, 127.0.0.1,*.intel.com"
	export auto_proxy=http://wpad.intel.com/wpad.dat
}

setup_cuda () {
	export CUDA_HOME=/usr/local/cuda
	export PATH=${CUDA_HOME}/bin:${PATH}
	export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${CUDA_HOME}/lib64
}

setup_vivado () {
	export VIVADO_HLS_DIR=/srv/Xilinx/Vivado/2017.2/linux64/Vivado_HLS/2017.2/bin
	export VIVADO_DIR=/srv/Xilinx/Vivado/2018.3/bin
	export PATH=${PATH}:${VIVADO_DIR}
	export XILINXD_LICENSE_FILE=2100@xilinx01p.elic.intel.com:2100@xilinx02p.elic.intel.com:2100@xilinx03p.elic.intel.com
}

setup_quartus () {
	if [ -z "$1" ] 
	then
		echo "Usage: setup_quartus <quartus_install_dir>"
		return -1 
	fi
	export LM_LICENSE_FILE="1800@altera02p.elic.intel.com"
	export PATH=${PATH}:${1}/quartus/bin:${1}/quartus/sopc_builder/bin:${1}/qsys/bin:${1}/syscon/bin
	export QUARTUS_ROOTDIR="$1/quartus"
	export QSYS_ROOTDIR="${1}/quartus/sopc_builder/bin"
}
complete -A directory setup_quartus

swmount () {
	sudo mount -t cifs --verbose -o username=dreddy,domain=AMR,vers=1\.0 //cmode.sj-itenvnas01.altera.com/swdev /swdev
}

vncsudo () {
	authfile=`sudo ls -1 /var/lib/xdm/authdir/authfiles`
	sudo x11vnc -repeat -bg -forever -xkb -rfbauth /home/dreddy/.vnc/passwd -auth /var/lib/xdm/authdir/authfiles/${authfile} -display :0
}

setup_local () {
	export PATH=${HOME}/.local/bin:${PATH}
	export LD_LIBRARY_PATH=${HOME}/.local/lib
}

setup_rust () {
	export PATH="$HOME/.cargo/bin:$PATH"
}

setup_go ()  {
	setup_proxy
	export GO111MODULE=on
	export GOPATH=${HOME}/.go
	export PATH=$PATH:$GOPATH/bin
}
 
setup_pyenv () {
	if [ -z "$1" ]
        then
                echo "Usage: setup_pyenv <env-name>"
                return -1
        fi
	export PATH=${PATH}:${HOME}/.local/bin
	if [ ! -d "${HOME}/Environments/$1" ]
	then
		mkdir -p ${HOME}/Environments/$1
		python3 -m venv ${HOME}/Environments/$1
	fi
	source ${HOME}/Environments/$1/bin/activate
	cd $HOME/Environments/$1
}

setup_aarch64 () {
	export PATH=${PATH}:/srv/toolchains/gcc-arm-9.2-2019.12-x86_64-aarch64-none-linux-gnu/bin
	export ARCH=arm
	export CROSS_COMPILE=aarch64-none-linux-gnu-
}

setup_llvm () {
	export PATH=/srv/llvm/bin:$PATH
	export LD_LIBRARY_PATH=/srv/llvm/lib:${LD_LIBRARY_PATH}
}

export PATH=$HOME/.local/bin:${PATH}
