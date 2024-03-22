PROMPT='%F{red}%/%f $ '
if [ "$TMUX" = "" ]; then tmux; fi
# alias
alias neofetch="neofetch --source /Users/kaiklemmer/.config/neofetch/neofetch_custom_ascii_logo"
neofetch
alias vim='nvim'
alias ezsh='nvim ~/.zshrc'
alias szsh='source ~/.zshrc'
alias ll='ls -alGp'
alias minictl="minikube kubectl --"
alias cdp='cd ~/programming'
alias cdg='cd ~/git'
alias evim='nvim ~/.config/nvim .'
# odin
export ODIN_HOME="/Users/kaiklemmer/git/Odin/odin"
export PATH="$ODIN_HIME:$PATH"
# odin end

# zig dev build for ziglings
export ZIG_HOME="/Users/kaiklemmer/programming/zig-0.11.0"
export PATH="$ZIG_HOME:$PATH"
# zig dev build end

# pnpm
export PNPM_HOME="/Users/kaiklemmer/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end

# For `brew`, `gh`, and other tools:
export PATH="${HOME}/.brew/bin:${PATH}"

# For `llvm`:
export PATH="$(brew --prefix llvm)/bin:${PATH}"
#export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"
# v14 is required for odin.
export PATH="/opt/homebrew/opt/llvm@14/bin:$PATH"


#.config/yabai/yabairc
#yabai --restart-service
