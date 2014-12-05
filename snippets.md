## Git
### Delete local branch
git branch -d branch_name
### Delete remote branch
git push origin :branch_name
### Ranges .. (double dot)
 - This should read: "What's not in origin/master, that is in master"
 - Or a shorthand: "Not in <> but in <>"
git log origin/master..master
### Ranges ... (triple dot)
 - This should read: "Of all the commits in both origin/master and master, which of those aren't shared by both"
 - Or a shorthand: "Not shared in both"
git log origin/master...master
