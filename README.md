# HikiWiki

This is probably not the wiki compiler you were looking for.

This is just my mad experiment in learning Haskell, so it misses many, many
features you will need IRL. Take a look at ikiwiki instead, it is far more
mature and useful.

That said, if you do want to try HikiWiki, for your own use or just to show
me how to write better Haskell, this file should get you started.

## Build

HikiWiki uses cabal. Use this shell script to cabal install and run tests:

    ./make.sh

## Install

It's too early to bother with packaging, so just copy the HikiWiki binary to
wherever you will serve your content from. You will also need to cp -r etc
alongside it to have the default config, templates and themes in place (needs
fixing soon).

## Setup

To create a new wiki:

```
scott@home:~$ ./HikiWiki init
What will the wiki be named?
myblog              
Initialized empty shared Git repository in /home/scott/myblog.git/
...
* [new branch]      master -> master
Pushing first commit.. done!
Installing post-update hook..
```

When the init command is done creating the new wiki, you will have:

```
scott@home:~$ tree myblog
myblog
├── index.mdwn
└── posts
    └── first_post.mdwn
```

This is the src directory. It's a clone of the myblog.git repo where you
will atually be pushing to (see below). HikiWiki compiles all markdown pulled
into this src repo as pat of the --rebuild command that is triggered after
each update. You don't need to worry about any of this, I'm just explaining
how it works.

```
scott@home:~$ cat myblog-config.yaml
# HikiWiki - YAML formatted config file

wikiname: myblog
srcdir: myblog
destdir: public_html/myblog
theme: cayman
```

This is the default config file for your new wiki. You can change the theme
by editing this file, specify another src dir (another repo, a fork, staging
area etc.), or change where this wiki publishes compiled content to (destdir).

```
myblog.git
```

This is the bare repo you want to add as a remote on whichever machines you
will be pushing content from. See below for more info on client setup.

```
public_html
```

Point your web server here, it's where HikiWiki writes all the HTML it has
compiled from the markdown you push to myblog.git.

## Client Setup

With HikiWiki installed on your content server and a new wiki created, all you
need to do on your client machines is clone your content repo, edit and create
your markdown files, then git commit and push them. HikiWiki runs on a
post-update hook, so it will compile the markdown you push to HTML and if your
web server is pointing at your wiki's destdir, you'll see your content go live
almost immediately after you have done your git push.

If I wanted to edit and push content to my new blog from my laptop, I'd do
something like this to get started:

```
git clone git://git.diskfish.org/myblog.git myblog-git
```

and then just edit and create new .md files in that repo, commit, and push.
