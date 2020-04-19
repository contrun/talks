#!/usr/bin/env bash
set -xeo nounset

if git diff --exit-code; then
        git pull --rebase=true
else
        git stash
        git pull --rebase=true
        git stash pop
fi
git add .
git commit
git status
read -r -p "Press enter to continue"
git push
