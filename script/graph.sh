find ./app/* ./formula ./src/* -name '*.hs' | xargs graphmod --no-cabal  > modules.dot 
