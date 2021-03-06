## Bash 
### Print a string multiple times (bit of a hack)
- ` printf 'HelloWorld\n%.0s' {1..5} `
- {1..5} is a range of args passed to printf command
- %.0s is a blank holder for the args, using %d would print the arg number as well. 
- source: [superuser.com/...](http://superuser.com/questions/86340/linux-command-to-repeat-a-string-n-times)

## Git

### Delete local branch
- git branch -d branch_name

### Delete remote branch
- git push origin :branch_name

### Clean untracked files (dangerous, take care)
- git clean -f 
##### for directories, too
- git clean -fd 

### Ranges .. (double dot)
- This should read: "What's not in origin/master, that is in master"
- Or a shorthand: "Not in <> but in <>"
- git log origin/master..master

### Ranges ... (triple dot)
- This should read: "Of all the commits in both origin/master and master, which of those aren't shared by both"
- Or a shorthand: "Not shared in both"
- git log origin/master...master

### Log lines/regex
- git log -L <start>,<end>:filespec
- git log -L /regex/:filespec


### Show each ref's last commit, sorted by most recent
- git for-each-ref --format='<what-to-output>' --sort='<by?>' refs/ | sort -r | column -t -s ',' 
- eg: `git for-each-ref --format='%(committerdate:short),%(objectname:short),(%(committerdate:relative)),%(authorname),%(refname:short)' --sort=authorname refs/ | sort -r | column -t -s ',' | grep Josh`

### ffmpeg
Share your screen via webcam in Linux...
- `ffmpeg -f x11grab -r 15 -s 1920x1080 -i :0.0+0,0 -vcodec rawvideo -pix_fmt yuv420p -threads 0 -f v4l2 /dev/video1`
