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
alias cf='cd $(find * -type d | fzf)'

export FZF_DEFAULT_OPTS="
--layout=reverse
--info=inline
--height=80%
--multi
--preview-window=:hidden
--preview '([[ -f {} ]] && (bat --style=numbers --color=always {} || cat {})) || ([[ -d {} ]] && (tree -C {} | less)) || echo {} 2> /dev/null | head -200'
--color='hl:148,hl+:154,pointer:032,marker:010,bg+:237,gutter:008'
--prompt='∼ ' --pointer='▶' --marker='✓'
--bind '?:toggle-preview'
--bind 'ctrl-e:execute(echo {+} | xargs -o nvim)'
--bind 'ctrl-y:execute(readlink -f {} | xclip -selection clipboard)'
--bind 'ctrl-alt-y:execute-silent(xclip -selection clipboard {})'
"


# zig dev build 
#export ZIG_HOME="/Users/kaiklemmer/programming/zig-macos-aarch64-0.14.0-dev.829+2e26cf83c"
#export PATH="$ZIG_HOME:$PATH"
# zig dev build end

# zig dev build for ziglings
export ZIG_HOME="/Users/kaiklemmer/programming/zig-macos-aarch64-0.14.0-dev.1002"
export PATH="$ZIG_HOME:$PATH"
# zig dev build end

# helix
export HELIX_HOME="/Users/kaiklemmer/helix"
export PATH="$HELIX_HOME:$PATH"
# helix end

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

#.config/yabai/yabairc
#yabai --restart-service

# bun completions
[ -s "/Users/kaiklemmer/.bun/_bun" ] && source "/Users/kaiklemmer/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
