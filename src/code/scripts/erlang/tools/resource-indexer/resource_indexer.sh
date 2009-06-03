#!/bin/sh

# Launcher for the Erlang resource indexer.

scan_dir=`pwd`

# Going where the script is available, as we found the needed BEAM from there:
indexer_dir=`dirname $0`

#echo "indexer_dir = $indexer_dir"

cd $indexer_dir


# Paths are specified both for source directories and for installation ones:
beam_paths=". ../../common/src ../../common/ebin"

script_options="$*"

#make -s resource_indexer.beam && 
erl +W w -- ${script_options} -pz ${beam_paths} +K true +A 8 -noshell -eval 'resource_indexer:main()' 

