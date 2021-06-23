# gitmanager
## Script to manage multiple repos with multiple branches

### Install instructions
* Install with `make install` to install program and add manpage entry.

or

* run the bash script `gitmanager` directly.



### KNOWN ISSUES
Auto cmds not currently working
Some shortcut flags not working properly


# Git Aliases #
# Aliases you might want to use from the .git_aliases file.
| ga | git add (Add changes) |
| gaa | git add all (Add all changes) |
| gaf | git add file (lists modified files in fzf and selected file will be added) |
| guf | git unstage file (lists modified files in fzf and selected file will be added) |
| --- | --- |
| gb | git branch (List local branches) |
| gba | git branch all (List all branches) |
| gbd | git branch delete [name] (Deletes a branch) |
| gbdf | git branch delete find (Opens banch list in fzf and deletes out the branch selected) |
| gbn | git branch new [name] (Creates a new branch and checks it out) |
| gbr | git branch remote (Lists all of the branches you have authored on the remote) |
| --- | --- |
| gc | git commit (commits staged changes) |
| gcm | git commit message (commits staged changes and prompts for commit message) |
| gca | git commit automatic (Commits staged changes with a very lazy automatic message) |
| --- | --- |
| gchk | git checkout [name] (Checks out the branch with the supplied name) |
| gchkf | git checkout find (Opens branch list in fzf and checks out the branch selected) |
| gchkaf | git checkout all find (Opens branch list including remotes in fzf and checks out the branch selected) |
| gchkn | git checkout number [number] (Checks out the branch with the supplied number) |
| gchkm | git checkout master (Checks out master) |
| --- | --- |
| grh | git reset head (Resets to head)  |
| grhh | git reset head hard (Hard resets to head discarding all changes) |
| grhs | git reset head soft (Soft resets to head)  |
| grf | git reset file (lists unstaged files in fzf and selected file will be reset to HEAD all changes discarded) |
| gclean | git clean (Removes untracked files)  |
| gfc | git full clean (Removes untracked files and all changes) |
| --- | --- |
| gd | git diff (Shows a diff of changes) |
| gdf | git diff file (lists unstaged files in fzf and a diff will be dispalyed for the selected file) |
| gdn | git diff name (Shows a diff of changes [name only]) |
| gdnm | git diff name master (Shows a diff of changes for master branch [name only]) |
| gds | git diff stat (Shows a diff of changes [name and line count]) |
| gdsm | git diff stat master(Shows a diff of changes against master branch [name and line count]) |
| --- | --- |
| gfo | git fetch origin (Fetch info on branches from origin) |
| gfop | git fetch origin prune (Fetch info on branches from origin and prune deleted) |
| --- | --- |
| gl | git log (Show the git log) |
| glo | git log oneline (Show the git log) |
| glo | git log oneline (Show the git log) |
| gld | git log decorated (Show the git log but fancy) |
| glm | git log me (Show my commits in the git log) |
| glmc | git log me compact (Show my commits in the git log compact form) |
| --- | --- |
| gm | git manage (Run git manage script) |
||
| gma | git merge abort (Aborts a git merge) |
| gmt | git merge test (Runs a test merge to see if there are any conflicts) |
| gmc | git merge conflicts (Opens conflict list in fzf then edits the selected file in vim) |
| gmom | git merge origin/master (Merge origin/master into current branch) |
| --- | --- |
| gpd | git pull down (git pull) |
| gpu | git push up (git push) |
| gpuf | git push up force (Force push) |
| gpuu | git push up upstream (git push and set the upstream for the remote) |
| --- | --- |
| gs | git status (git status) |
| --- | --- |
| gsl | git stash list (Lists the stashes) |
| gsp | git stash pop (Pops the stash) |
| gss | git stash show (Shows changes in the last stash) |
| --- | --- |
| gv | git version (find prev version of file) |
| --- | --- |
| gh | git help (Displays this message) |
