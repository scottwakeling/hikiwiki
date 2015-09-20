# TODO

## Bugs

No error message is given if pandoc is not installed

--init should end with a rebuild, ready to go..
- install HikiWiki as part of build
  (post-update can then HikiWiki, not ./HikiWiki, and
   won't need hacky etc folder laying around, it can go in ~/.hikiwiki/etc)

Going to have:
  ~/.hikiwiki/wikilist|(etc/setup|themes)|plugins
Not going to have /etc/.hikiwiki until you explicitly handle sudo invocation

.hikiwiki and etc/, wikilist location etc. needs to be dynamic
    which first requires etc is installed in .hikiwiki by cabal install

## Templating

Templating with Heist?


## Install

cabal install
    So I don't have to cp out of hikiwiki into ~
    Creates .hikiwiki/etc and .hikiwiki/wikilist


## Documentation

Add a Files seciton to man page
  wikilist
  /etc


## Debian

Package for Debian


## Testing

Improve the command line parsing (RWH late chapter)

hikiwiki rebuild repo
  loads config from repo.setup and compile srcdir into dstdir
  if wiki not registered, rebuild that local src dir if present (staging/test)

staging area
  updating website/wiki/blog with git is great, but some projects might
    require a staging area because pushing straight to 'production' is scary!
  So, if --rebuild srcDir is not enough, you need to be able to clone your
    production wiki out and use --init to create a new remote and dst dir in
    place for a staging area people can clone from and push to..



## Plugins

hikiwiki theme install foo
hikiwiki plugin install bar
  lightweight 'package' system
    'hikis'
    user-wide, and system-wide hiki DBs
    hpkg cmd line tool hpkg -i etc.
    hiki server/hpkg config to point at mirrors etc.
    hpkg rollback .. interesting.. the DBs are git repos..?
    you'd learn a lot from this










