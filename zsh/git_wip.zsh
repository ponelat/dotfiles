
# Override the oh-my-zsh implementation ( copy/paste )
function git_prompt_info() {
  local ref
  if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" != "1" ]]; then
    ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX $(git_wip_prompt)"
  fi
}

## Returns WIP ( in red ), if the last commit has the word 'wip' in it
function git_wip_prompt() {
  local ref
  git log --pretty=format:"%s" -1 | grep '\<wip\>' >/dev/null 2>&1
  ref=$?
  if [[ $ref == 0 ]]; then
    echo "%{$fg[red]%}WIP%{$reset_color%}"
  fi
}
