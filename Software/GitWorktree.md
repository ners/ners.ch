# Git Worktree

Git Worktree puts branches into subdirectories, allowing us to work on multiple branches at the same time.

```bash
mkdir nixpkgs
git clone --bare git@github.com:ners/nixpkgs .bare
echo "gitdir: ./.bare" > .git
git remote add upstream git@github.com:nixos/nixpkgs
```
