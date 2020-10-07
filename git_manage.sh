#!/bin/bash
options=("Search for git repos" "Check for changes" "List changes" "Pull and push" "Pull and push auto" "Add all changes + commit + push" "Clean old branches" "Merge origin/master into branches" "List branches" "Compare master" "Compare remote" "Quit")

if [ -t 1 ] && command -v tput > /dev/null; then
    # see if it supports colors
    ncolors=$(tput colors)
    if [ -n "$ncolors" ] && [ $ncolors -ge 8 ]; then
        bold="$(tput bold       || echo)"
	blink="$(tput blink      || echo)"
        reset="$(tput sgr0      || echo)"
        black="$(tput setaf 0   || echo)"
        red="$(tput setaf 1     || echo)"
        green="$(tput setaf 2   || echo)"
        yellow="$(tput setaf 3  || echo)"
        blue="$(tput setaf 4    || echo)"
        magenta="$(tput setaf 5 || echo)"
        cyan="$(tput setaf 6    || echo)"
        white="$(tput setaf 7   || echo)"
    fi
fi

repos=""
auto=false

#Current working direcroty
cwd="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

#Displays the option menu (if no args provided to program)
show_menu() {
    select option in "${options[@]}"; do
        echo $option
        break
    done
}

##Sets repos variable using cache files
set_repos() { repos=$(cat ${cwd}/repos.cache | grep -v -f ${cwd}/repos.exclude); }

##Sets repos and prints them to the user
using_repos() { set_repos && echo "Using repos:${cyan}$(cat ${cwd}/repos.cache | grep -vf ${cwd}/repos.exclude)${reset}"; }

#pwd but coloured
colour_pwd() { echo ${cyan}$(pwd)${reset}; }

#path bub coloured
colour_path() { echo ${cyan}${path}${reset}; }

#Used to make all calls to branch use the same formatting.
git_branch() { git branch; }

#Used to make all calls to commit use the same formatting.
git_commit() { read -p "Please enter commit message: " && git commit -m "$REPLY"; }

#Used to make all calls to push use the same formatting.
git_push() { echo "${magenta}Pushing...${reset}" && git push && echo; }

#Used to make all calls to pull use the same formatting.
git_pull() { echo "${magenta}Pulling...${reset}" && git pull; }

#Used to make all calls to merge use the same formatting.
git_merge() { echo "${magenta}Merging...${reset}" && git merge $1; }

#Used to make all calls to merge use the same formatting.
git_merge_abort() { echo "${magenta}Aborting Merge...${reset}" && git merge --abort; }

#Used to make all calls to rebase use the same formatting.
git_rebase() { echo "${magenta}Rebasing...${reset}" && git rebase $1; }

#Used to make all calls to checkout use the same formatting.
git_checkout() { git checkout $1 2>&1 1>/dev/null | sed -E "s/(.+')(.+)('.*)/\1${blue}\2${reset}\3/"; }

#Fetches a git repo
git_fetch() { cd $1 && echo -e "${magenta}Fetching...${reset} $(colour_pwd)$(git fetch origin --prune 2>&1 >/dev/null)\n"; }

#Fetches all repos
git_fetch_all_repos() { for path in $repos; do git_fetch "$path/.."; done }

#Calls git status with ability to use cached value
git_status() {
#    if [[ "$1" == "CACHED" ]]; then 
#	echo $status
#	return
#    fi
#    status=$(git status | sed "s/\n/\\n/")
#    status=$(git status)
#    echo $status
	git status
}

#Checks if the current branch working tree is clean. arg1 can be used to toggle CACHED
tree_is_clean() { git_status $1 | grep -q "nothing to commit, working tree clean" && return 0 || return 1; }

#Checks if the current branch is up to date. arg1 can be used to toggle CACHED
branch_up_to_date() { git_status $1 | grep -q "Your branch is up to date with " && return 0 || return 1; }

#Checks if the current branch has conflicts. arg1 can be used to toggle CACHED
has_conflicts() { git_status $1 | grep -Eq "both added|both modified" && return 0 || return 1; }

#Provides a one line summary on the current repo/branch
git_summary() {
    tree_is_clean && branch_up_to_date "CACHED" && echo "$(colour_pwd): ${green}No changes found${reset}" && return 0 
    tree_is_clean "CACHED" && echo "$(colour_pwd): ${yellow}Out of sync${reset}" && return 1 
    has_conflicts "CACHED" && echo "$(colour_pwd): ${red}Merge conflict detected${reset}" && return 2
    echo "$(colour_pwd): ${red}Changes detected${reset}" && return 3
}

#Checks out master and updates list of branches
git_get_branches() {
    git_checkout master
    branches=$(git for-each-ref --format='%(refname)' refs/heads/ | sed "s|refs/heads/||")
    echo -e "\nFound branches:\n$(echo ${blue}${branches}${reset} | tr " " "\n")\n"
}

#Promts the user to answer a yes/no question. 
#Returns after a single char is entered without hitting return.
ask() {
    read -p "${1} ${yellow}y/n${reset} " -n 1 -r
    echo
    [[ $REPLY =~ ^[Yy]$ ]] && return 0 || return 1
}

#Asks the user if they want to push then pushes and shows status.
ask_if_push() {
    git_checkout $branch | grep -q "but the upstream is gone." && echo -e "Remote has been deleted. Pushing will recreate it.\n"
    ask "push" && git_push && git_status && echo 
}

#Checks if a repo should be skipped
check_should_skip_repo() {
    [[ "$auto" == true ]] || ask "Update project $(colour_path)" || return 1
    git_fetch "$path/.."
    ask_if_skip_dirty && return 0 || echo; return 1
}

#Prompts the user to skip if the current branch is dirty
ask_if_skip_dirty() {
    tree_is_clean && return 0
    git_status
    ask "Working tree is not clean, would you like to skip this project (y to skip, n to recheck)" && return 1
    ask_if_skip_dirty
}

#Prompts the user to skip if the current branch is dirty or revert the merge and continue
ask_if_skip_dirty_merge() {
    tree_is_clean && return 0
    git_status
    echo    "${red}Auto merge failed, conflicts found${reset}."
    echo    "Would you like to skip remaining branches in this project or revert the merge?" 
    read -p "(y to skip, n to recheck, r to revert merge and continue) ${yellow}y/n/r${reset} " -n 1 -r
    echo -e "\n\n"
    [[ $REPLY =~ ^[Yy]$ ]] && return 1
    [[ $REPLY =~ ^[Rr]$ ]] && git_merge_abort
    ask_if_skip_dirty_merge 
}

#Checks how far ahead/behind a branch is
get_ahead_behind() {
    git_upstream_status_delta=$(git rev-list --left-right ${1}...${2} -- 2>/dev/null)
    AHEAD=$(echo $git_upstream_status_delta | grep -c '^<');
    BEHIND=$(echo $git_upstream_status_delta | grep -c '^>');
    [[ "$AHEAD" == 0 ]] && ahead="${green}$AHEAD${reset}" || ahead="${red}$AHEAD${reset}"
    [[ "$BEHIND" == 0 ]] && behind="${green}$BEHIND${reset}" || behind="${red}$BEHIND${reset}"
}


########################################################
#######################  CMDS  #########################
########################################################


#Finds repos on the system
find_repos() {
    cd $1
    find ~+ -name .git -type d -prune 2> /dev/null  | grep -v -f ${cwd}/repos.cache >> ${cwd}/repos.cache
    echo -e "Updated repos.cache\n"
    
    if [[ ! -e ${cwd}/repos.cache ]]; then
        echo "Cannot find repos.cache. Exiting..."
        read -p "Press enter to continue"
        exit 1
    fi
    echo "Found the following repos (in repos.cache)"
    cat ${cwd}/repos.cache
    echo
    if [[ ! -e ${cwd}/repos.exclude ]]; then
        echo "Cannot find repos.exclude"
        touch ${cwd}/repos.exclude
        echo -e "Created repos.exclude\nCopy the path of any unwanted repos in the above output to a new line of this file.\n"
        read -p "Press enter to continue once you have completed this step"
    fi
    echo "Excluding the following repos (in repos.exclude)"
    cat ${cwd}/repos.exclude
    echo -e "\nFinal list"
    cat ${cwd}/repos.cache | grep -vf ${cwd}/repos.exclude
    repos=$(cat ${cwd}/repos.cache | grep -v -f ${cwd}/repos.exclude)
}

#Displays a brief summary of all repos
repos_summary() { for path in $repos; do cd "$path/.."; git_summary; done | column -t -c 1 -s ":"; }

#Displays the status of all repos
repos_status() {
    for path in $repos; do
        cd "$path/.."
        git_summary
	git_status
	echo -e '\n'
    done
}

#Compares all the branches of all the repos to a specific branch
compare_branches() {
    [ -z "$1" ] && continue;
    for path in $repos; do
        cd "$path/.."
        header=$(echo -e "----------------------------------------~-----< $(pwd | xargs basename) >-----~----------------------------------------")
        footer=$(echo -e "________________________________________~______________________~________________________________________")
        git for-each-ref --format="%(refname:short) %(upstream:short)" refs/heads | \
        while read local remote; do 
            [[ $1 == "origin/master" ]] && remote="origin/master"
	    get_ahead_behind $local $remote
            echo -e "${local}~(ahead ${ahead}) | (behind ${behind})~$remote\n"
        done | echo -e "${header}\n$(cat)\n${footer}"
    done | cat | column -t -c 1 -s "~"
    echo
}

#Commit changes and push in all branches
commit_and_push() {
    for path in $repos; do
        cd "$path/.."
	git_summary && continue
	[[ $? == 1 ]] && continue
        git_status
        #prompt for diff unless NODIFF specified
        [[ $1 != "NODIFF" ]] && ask "Show diff" && echo -e "$(git diff --color=always)\n"

        if ask "Stage all changes + commit"; then
            git add -A
            git_status
	    git_commit
            git_status
            ask_if_push
        fi
        echo -e "$(git_status)\n\n"
    done
}

#Rebase all branches
rebase_branches() {
    for path in $repos; do
        check_should_skip_repo && git_get_branches || continue
        for branch in $branches; do
            git_checkout
            ask "Rebase this branch on $1" || echo -e "Skipping...\n"; continue
	    git_rebase
            ask_if_skip_dirty_merge || continue
	    ask_if_push 
        done
    done
}

#Merge a branch into all branches
merge_into_branches() {
    for path in $repos; do
        check_should_skip_repo && git_get_branches || continue
        for branch in $branches; do
            git_checkout
            ask "Merge origin/master into this branch" || echo "Skipping..."; continue
            git_merge
            ask_if_skip_dirty_merge || continue
	    ask_if_push 
        done
    done
}

#Clean old branches already merged into master
clean_branches() {
    for path in $repos; do
       check_should_skip_repo && git_get_branches || continue
       for branch in $branches; do
           get_ahead_behind "$branch" "origin/master"
           printf "$branch (ahead $ahead) | (behind $behind) origin/master\n"
           if [[ "$AHEAD" == 0 ]] && ask "This branch is ${green}0${reset} commits ahead of origin/master. Would you like to delete it"; then
               echo -e "\n$(git branch -D $branch)\n"
           else
               echo -e "\n\n"
           fi
       done
    done
}

#push pull all branches
push_pull() {
    for path in $repos; do
        check_should_skip_repo || continue
        git_get_branches 
        for branch in $branches; do
            git_checkout $branch
            [[ "$auto" == true ]] || ask "pull+push" || continue
	    git_pull
            ask_if_skip_dirty_merge || continue
	    git_push
        done
    done
}

#List all branches in all repos
list_branches() {
    for path in $repos; do
        echo "Branches in $(colour_path)"
        cd ${path}/..
        git_branch
    done
}

#Bring everything up to date
everything() {
    for path in $repos; do
        [[ "$auto" == true ]] || ask "Update project $(colour_path)" || continue
        git_fetch "$path/.."
	git_get_branches
        for branch in $branches; do
	    git_summary && continue
	    ret="$?"
            #If out of sync then pull push
	    if [[ "$ret" == 1 ]]; then
                [[ "$auto" == true ]] || ask "pull+push" || continue
	        git_pull
                ask_if_skip_dirty_merge || continue
	        git_push
            #If conflicts detected ask to skip (auto skip if auto enabled)
            elif [[ "$ret" == 2 ]]; then
                [[ "$auto" == true ]] && continue
                ask_if_skip_dirty_merge || continue
	        git_push
            #If changes detected, commit them
            elif [[ "$ret" == 3 ]]; then
                git_status
                #prompt for diff unless NODIFF specified
                [[ $1 != "NODIFF" ]] && ask "Show diff" && echo -e "$(git diff --color=always)\n"
                if ask "Stage all changes + commit"; then
                    git add -A
                    git_status
	            git_commit
                    git_status
                    ask_if_push
                fi
                echo -e "$(git_status)\n\n"
	    fi
        done
    done
}


#Prints the help function
show_help() {
    echo    "Usage: gitmanage [OPTION]..."
    echo -e "Used to manage multiple branches across multiple git repositories\n"

    echo    "  -F          Search for git repos" 
    echo -e "              Searches C: drive for git repos"
    echo -e "              (This must completed at least once to update the cache used by other functions)\n"

    echo    "  -f          Fetch repos" 
    echo -e "              Fetch and prune all repos\n"

    echo    "  -s          Check for changes"
    echo -e "              Checks each repository for changes which have not been comitted and provides a simple summary\n"

    echo    "  -S          List changes"
    echo -e "              List all changes in each repository which have not been comitted\n"

    echo    "  -p          Pull and push" 
    echo -e "              Prompts you to pull+push each branch in your git repositories\n"

    echo    "  -P          Pull and push auto" 
    echo -e "              Prompts you to pull+push each branch in your git repositories without asking confirmation for each branch\n"

    echo    "  -c          Add all changes + commit + push" 
    echo -e "              Prompts you to add all changes + commit + push each git repository\n"

    echo    "  -C          Add all changes + commit + push NO DIFF" 
    echo -e "              Prompts you to add all changes + commit + push each git repository but will not prompt to show diff\n"

    echo    "  -b clean    Clean old branches" 
    echo -e "              Prompts you to delete any branches in your git repositories which are 0 commits ahead of origin/master\n"

    echo    "  -m          Merge origin/master into branches" 
    echo -e "              Prompts you to merge origin/master into each branch in you git repositories\n"

    echo    "  -r          Rebase branches" 
    echo -e "              Prompts you to rebase into each branch on the target branch in you git repositories\n"

    echo    "  -b          List branches" 
    echo -e "              Lists all the local branches in your git repositories\n"

    echo    "  -ba         List all branches including remotes" 
    echo -e "              Lists all the branches in your git repositories\n"

    echo    "  -b master   Compare master" 
    echo -e "              Compares each branch in your git repositories against origin/master\n"

    echo    "  -B master   Compare master no fetch" 
    echo -e "              Compares each branch in your git repositories against origin/master\n"

    echo    "  -b remote   Compare remote" 
    echo -e "              Compares each branch in your git repositories against it's remote branch\n"

    echo    "  -B remote   Compare remote no fetch" 
    echo -e "              Compares each branch in your git repositories against it's remote branch\n"

    echo    "  -h          Help" 
    echo -e "              Displays this message"
}


#Process args
while getopts ":f::F::s::S::p::P::c::C::m::M::r::b::B::e::E::h:" opt; do
    case $opt in
        b)
            set_repos
            if [[ $OPTARG == "clean" ]]; then clean_branches
	    fi
            git_fetch_all_repos 
            if [[ $OPTARG == "master" ]]; then compare_branches "origin/master"
            elif [[ $OPTARG == "remote" ]]; then compare_branches "remote"
            else
                compare_branches "$OPTARG"
            fi
            exit
        ;;
        B)
            set_repos
            if [[ $OPTARG == "master" ]]; then compare_branches "origin/master"
            elif [[ $OPTARG == "remote" ]]; then compare_branches "remote"
            else
                compare_branches "$OPTARG"
            fi
            exit
        ;;
        F)
            if [[ $OPTARG == "clean" ]]; then echo "TODO Clean repo.cache files"
            else
                find_repos $OPTARG
            fi
            exit
        ;;
        r)
            set_repos
	    git_fetch_all_repos 
            if [[ $OPTARG == "master" ]]; then rebase_branches "origin/master"
            else
                rebase_branches "$OPTARG"
            fi
            exit
        ;;
        \?) #invalid flag
            echo "Invalid option: -$OPTARG" >&2
            exit
        ;;
        :) #arg missing param
            if [[ $OPTARG == "F" ]]; then find_repos "/c"
            else
                set_repos
                if   [[ $OPTARG == "f" ]]; then git_fetch_all_repos
                elif [[ $OPTARG == "s" ]]; then repos_summary
                elif [[ $OPTARG == "S" ]]; then repos_status
                elif [[ $OPTARG == "p" ]]; then push_pull
                elif [[ $OPTARG == "P" ]]; then auto=true; push_pull
                elif [[ $OPTARG == "c" ]]; then commit_and_push
                elif [[ $OPTARG == "C" ]]; then commit_and_push "NODIFF"
                elif [[ $OPTARG == "m" ]]; then echo "TODO deted merge conflicts"
                elif [[ $OPTARG == "M" ]]; then merge_into_branches
                elif [[ $OPTARG == "r" ]]; then rebase_branches "origin/master"
                elif [[ $OPTARG == "b" ]]; then list_branches
                elif [[ $OPTARG == "B" ]]; then echo "TODO"
                elif [[ $OPTARG == "e" ]]; then everything
		elif [[ $OPTARG == "E" ]]; then auto=true; everything 
                elif [[ $OPTARG == "h" ]]; then show_help
                fi
            fi
            exit
        ;;
    esac
done


#Show menu
show_help
while true; do
    opt=$(show_menu)
    
    if [[ $opt == "Search for git repos" ]]; then
       echo -e "\nFinding git repos on C:"
       find_repos "/c"
    elif [[ $opt == "Quit" ]]; then
        break
    else
        echo
        using_repos
    fi

    if [[ $opt == "Pull and push" ]]; then
        echo -e "\n${yellow}Prompts you to pull+push each branch in you git repositories\n${reset}"
        push_pull
    elif [[ $opt == "Pull and push auto" ]]; then
        echo -e "\n${yellow}Prompts you to pull+push each branch in you git repositories\n${reset}"
	auto=true
        push_pull
    elif [[ $opt == "Add all changes + commit + push" ]]; then
        echo -e "\n${yellow}Prompts you to Add all changes + commit + push each of you git repositories\n${reset}"
        commit_and_push
    elif [[ $opt == "Clean old branches" ]]; then
        echo -e "\n${yellow}Prompts you to delete any branches in your git repositories which are 0 commits ahead of master\n${reset}"
        clean_branches
    elif [[ $opt == "List branches" ]]; then
        echo -e "\n${yellow}Lists all the local branches in your git repositories\n${reset}"
        list_branches
    elif [[ $opt == "Compare master" ]]; then
        echo -e "\n${yellow}Compares each branch in your git repositories against origin/master\n${reset}"
        git_fetch_all_repos 
        compare_branches "origin/master"
    elif [[ $opt == "Compare remote" ]]; then
        echo -e "\n${yellow}Compares each branch in your git repositories against it's remote branch\n${reset}"
        git_fetch_all_repos 
        compare_branches "remote"
    elif [[ $opt == "Check for changes" ]]; then
        echo -e "\n${yellow}Checks each repository for changes which have not been comitted\n${reset}"
        repos_summary
    elif [[ $opt == "List changes" ]]; then
        echo -e "\n${yellow}Checks each repository for changes which have not been comitted\n${reset}"
        repos_status
    elif [[ $opt == "Merge origin/master into branches" ]]; then
        echo -e "\n${yellow}Prompts you to merge origin/master into each branch in you git repositories\n${reset}"
        merge_into_branches
    fi
    echo
done
