#!/bin/sh

# Launcher for the Erlang resource indexer.

beam_paths=". ../../common/src"

make -s resource_indexer.beam && erl +W w -- $* -pz ${beam_paths} +K true +A 8 -noshell -eval 'resource_indexer:main()' 
