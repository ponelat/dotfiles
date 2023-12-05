#!/usr/bin/env bash

function each-remote-branch() {
  git for-each-ref --format='%(refname:short)' refs/remotes/
}

# Loop through each folder in the specified directory
for dir in $1/*; do
    if [ -d "$dir" ]; then
        cd "$dir"

        # Check if directory is a git repository
        if ! git rev-parse --git-dir > /dev/null 2>&1; then
          echo "$dir (Not a git repo)"
        else

          # Check if git is dirty
          if [ -n "$(git status --porcelain)" ]; then
              header="$dir (Dirty)"
          else
              header="$dir (Clean)"
          fi

          # List unpushed commit for each branch
          body=$(git log --pretty=format:'%C(auto)%h %d %s' --branches --not --remotes)

          # Check if git repo has no remotes
          if [ -z "$(git remote)" ]; then
              header="$header (No remotes)"
          fi

          # Print header and body if body is not empty else if header contains "Dirty", print just that
          if [ -n "$body" ]; then
              echo "$header (Unpushed):"
              # Indent the body
              # Print ascii yellow color
              echo -e "\e[33m$body\e[0m" | sed 's/^/\t/'

          elif [[ "$header" == *"Dirty"* || "$header" == *"No remotes"* ]]; then
              echo "$header"
          fi
        fi
    fi
done
