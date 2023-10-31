rm -rf graph
go () {
    mkdir -p $(dirname graph/$1)
    find $1 -name '*.hs' | xargs graphmod --no-cabal > graph/$1.dot 
}
export -f go
find app formula src -type d -exec bash -c 'go "$@"' bash {} ';'
find ./app/* ./formula ./src/* -name '*.hs' | xargs graphmod --no-cabal  > graph/modules.dot 
