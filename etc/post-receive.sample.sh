#!/bin/bash

# necessary on archlinux, at least, as the locale was not picked up otherwise
export LANG=en_US.UTF-8

if [ -n $GIT_DIR ]; then
        unset GIT_DIR
        cd ..
fi

# force checkout
git checkout -f

# build site binary (if no change it won't build)
ghc --make bin/site

#backup current state for quick recovery, just in case
rm -rf _previous 
cp -r _site _previous 

# build site to _site directory
bin/site build

# ensure we have the correct link on initial build
# (commented out option force overrides)
# ln -sfn _site _live
if [ ! -h _live ]; then
        ln -s _site _live;
fi
