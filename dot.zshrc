
# Set path if required
#export PATH=$GOPATH/bin:/usr/local/go/bin:$PATH

# Aliases
alias ls='ls -F'
alias ll='ls -lah --color=auto'
alias grep='grep --color=auto'
alias ec="$EDITOR $HOME/.zshrc" # edit .zshrc
alias sc="source $HOME/.zshrc"  # reload zsh configuration

# Set up the prompt - 
autoload -Uz promptinit
promptinit

# Use vi keybindings even if our EDITOR is set to vi
bindkey -e

setopt histignorealldups sharehistory

# Keep 5000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit


#%K{blue}%n@%m%k %B%F{cyan}%(4~|...|)%3~%F{white} %# %b%f%k
# Define the theme
prompt_mytheme_setup() {
  PS1="%n@%m:[%F{cyan}%~%f]%# "
}

# Add the theme to promptsys
prompt_themes+=( mytheme )

# Load the theme
prompt mytheme

# bc base conversions

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

setup_pyenv () {
	if [ -z "$1" ]
        then
                echo "Usage: setup_pyenv <env-name>"
                return -1
        fi
	export PATH=${PATH}:${HOME}/.local/bin
	if [ ! -d "${HOME}/Venv/$1" ]
	then
		mkdir -p ${HOME}/Venv/$1
		python3 -m venv ${HOME}/Venv/$1
	fi
	source ${HOME}/Venv/$1/bin/activate   # commented out by conda initialize
	cd $HOME/Venv/$1
}

export PATH=$HOME/.local/bin:${PATH}
