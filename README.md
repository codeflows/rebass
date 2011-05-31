Rebass
======

File sync tool  for music projects with automatic bandwidth saving.. It's in the works.

Uses LAME for mp3 encoding.

Installation (OSX)
------------------

- brew install ghc haskell-platform lame
- cabal update
- cabal install

Try it out
---------- 

./rebass

Backlog
-------

- Merge remote changes back to local project
- Test with a real-life project
- Acceptance tests: parse -> serialize -> compare to original for actual Reaper project files
