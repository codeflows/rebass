Rebass
======

File sync tool for Reaper music projects:
- "rebass init lol.RPP lol" copies your project to a directory under ~/Dropbox/lol
- It copies also all of the project samples to the same directory, with
  MP3 compression
- "rebass update lol.RPP" updates your changes to the project into the Dropbox location
  (the location was stored by "init" command)
- TODO: rebass import for importing a project from Dropbox
- TODO: rebass update to support pulling remote changes from Dropbox

Written entirely in Haskell. Uses LAME for mp3 encoding.

Installation (OSX)
------------------

- brew install ghc haskell-platform lame
- cabal update
- cabal install

Try it out
---------- 

- export PATH=$PATH:~/.cabal/bin
- rebass

Backlog
-------

- Merge remote changes back to local project
- Parse binary stuff, as in examples/RealLifeProject.RPP
- Acceptance tests: parse -> serialize -> compare to original for actual Reaper project files
