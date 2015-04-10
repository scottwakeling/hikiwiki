# TODO

## Themes

Templating with Heist?

Be able to specify a theme to wrap generated html in


## Documentation

Add a Files seciton to man page
  wikilist
  /etc


## Configuration

Source and Publish dirs need to be absolute paths in config yaml


## Bugs

Irrefutable pattern passing HikiWiki unrecognised cmd line args

No error message is given if pandoc is not installed
    Is this still an issue? .cabal require it?

--init should end with a rebuild, ready to go..

- install HikiWiki as part of build
  (post-update can then HikiWiki, not ./HikiWiki, and
   won't need hacky etc folder laying around, it can go in ~/.hikiwiki/etc)

Going to have:
  ~/.hikiwiki/wikilist|etc|themese|plugins
Not going to have /etc/.hikiwiki until you explicitly handle sudo invocation


## Testing

I like :reload in ghci to see if it's compiling
  but running a simple test suite as part of make.sh would be even better!

Improve the command line parsing (RWH late chapter)

hikiwiki --rebuild repo
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










