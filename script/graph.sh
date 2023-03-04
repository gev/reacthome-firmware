find ./app/* ./formula ./src/* -name '*.hs' | xargs graphmod --no-cabal -p  > modules.dot 
