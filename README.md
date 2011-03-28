Rebass
======

File sync tool  for music projects with automatic bandwidth saving.. It's in the works.

Uses LAME for mp3 encoding.

Installation (OSX)
------------------

brew install ghc haskell-platform lame
cabal update
cabal install MissingH hspec

Try it out
---------- 

./rebass

Backlog
-------

- Modify Reaper project file, converting wav references to mp3 ones
- Collect media files listed in project file into one location
- Add makefile and packaging
- Support chunks of data inside nodes (they seem base64-encoded?)
- Acceptance tests: parse -> serialize -> compare to original for actual Reaper project files
- Split stuff under src/ and test/
