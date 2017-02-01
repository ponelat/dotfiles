#!/bin/sh

git log --pretty=format:"%s" -1 2>/dev/null | grep '\<wip:\?\>'
