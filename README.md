ethanschoonover.com site source
===============================

I use [Hakyll](http://jaspervdj.be/hakyll/) to maintain this site. See the 
`bin` directory for the site.hs which builds the site statically.

Source structure
----------------

    ├── bin         hakyll source code
    ├── etc         nginx local config files
    ├── files       downloads, global to site
    ├── pages       special pages (colophon, error pages)
    │   └── errors
    ├── posts       one time posts, with possible updates
    ├── projects    ongoing, updated projects
    ├── resources   static resources
    │   ├── css
    │   ├── img
    │   └── js
    ├── src         non hakyll source files
    │   └── psd
    └── templates   templates used by hakyll

Deployment process overview
---------------------------

Local site modifications are tested until correct and pushed to github for 
offsite storage. When site updates are ready for public deployment I push to my 
web server.

Some subdirectories, notably the project subdirectories, are managed using 
git-subtree. The web pages for those project are identical to the github 
repository for those projects. The Hakyll site.hs code processes them and 
converts the github specific absolute urls to be relative to my server's 
webroot.

Git subtree has the advantage of allowing me to work on the project from either 
the main project directory on my machine or from the website directory (for 
local site preview). Additionally, the project itself is maintained with 
several git-subtree subdirectories, allowing me to break out the vim 
subdirectory as it's own repository and work on it, for instance, from within 
my personal dotfiles.

See <http://chrisdone.com/posts/2010-04-04-hakyll-and-git-for-you-blog.html> 
for other details on a similar hakyll-github-server deployment process that 
I took inspiration from.

Key commands
------------

* Building Hakyll site.hs

    `ghc --make bin/site`

* Clean and preview site locally. `bin` is in my path so I can just use `site` 
  and it will source locally.

    `site clean`
    `site preview`
    `site preview 8900` (alternate port)

* Create git-subtree repo (my own standardized naming conventions)

    * Add remotes:

        git remote add remote-solarized \
        git@github.com:altercation/solarized.git

    * Add subtrees:

        git subtree add --prefix=projects/solarized \
        remote-solarized master

    * Pull changes into local subtree

        git subtree pull --prefix=projects/solarized \
        remote-solarized master

    * Push changes from local subtree

        git subtree push --prefix=projects/solarized \
        remote-solarized master

* Push to github

    standard commit and push

* Deploy to live site

    git push aithops

Deployment Server
-----------------

* Site root and config

        /srv/www
        ├── commons
        │   └── errors
        │       ├── 403
        │       ├── 404
        │       └── 50x
        ├── domains
        │   └── domain.tld
        ├── etc
        └── logs

* Create repository and configure on remote

    www@aithops$ mkdir ethanschoonover.com
    www@aithops$ cd ethanschoonover.com
    www@aithops$ git init
    www@aithops$ git config receive.denyCurrentBranch 'ignore'
    www@aithops$ cd ..
    www@aithops$ chmod -R g+w ethanschoonover.com

    (last command only required as I'll be commiting as a different user that
    is also a member of the www group)

* Post-receive Hook

    ethanschoonover.com/.git/hooks/post-receive file (ensure this is set to 
    executable):

        #!/bin/bash

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
        # note: this is not a full rebuild, but could be
        # if we did a relink to the _previous directory prior
        # to the rebuild, then a link back to _site
        bin/site build

        # ensure we have the correct link on initial build
        # (commented out option force overrides)
        # ln -sfn _site _live
        if [ ! -h _live ]; then
                ln -s _site _live;
        fi

* Commit to server

        $ git push aithops

Notes
-----

See my dotfile repository for my master gitignore file. I've added both _cache 
and _site there (along with the standard haskell ignores) and locally I've 
added bin/site (the compile hakyll binary).

