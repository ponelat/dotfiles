#!/usr/bin/env bash

function each-remote-branch() {
  git for-each-ref --format='%(refname:short)' refs/remotes/
}

# Loop through each folder in the specified directory
for dir in $1/*; do
    if [ -d "$dir" ]; then
        cd "$dir"

	has_issue=""

        # Check if directory is a git repository
        if ! git rev-parse --git-dir > /dev/null 2>&1; then
	    has_issue="not-git"
	    echo "$dir (Not a git repo)"
	else

          # Check if git is dirty
          if [ -n "$(git status --porcelain)" ]; then
	      has_issue="dirty-git"
              header="$dir (Dirty)"
          else
              header="$dir (Clean)"
          fi

          # List unpushed commit for each branch
          body=$(git log --pretty=format:'%C(auto)%h %d %s' --branches --not --remotes)

          # Check if git repo has no remotes
          if [ -z "$(git remote)" ]; then
	      has_issue="no-remotes"
              header="$header (No remotes)"
          fi

          # Print header and body if body is not empty else if header contains "Dirty", print just that
          if [ -n "$body" ]; then
              echo "$header (Unpushed):"
              # Indent the body
              # Print ascii yellow color
              echo -e "\e[33m$body\e[0m" | sed 's/^/\t/'
	      has_issue="unpushed"

          elif [[ "$header" == *"Dirty"* || "$header" == *"No remotes"* ]]; then
	      has_issue="dirty-or-no-remotes"
              echo "$header"
          fi

	  # Check if there is an issue and if there is the --one flag. If so, exit early
	  if [ -n "$has_issue" ] && [ "$2" == "--one" ]; then
	      echo "Only checking one repo. Exiting..."
	      exit 1
	  fi

        fi
    fi
done
