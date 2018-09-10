#!/bin/bash
set -e
set -x


echo $CC
echo $CXX

cd $TRAVIS_BUILD_DIR/build
#    - export MAKEFLAGS='-j2'
python -c "import numpy as n; print(n.__version__); print(n.get_include());"
cmake --build . --config ${TRAVIS_BUILD_TYPE} --target all -- -j2

#    - export MAKEFLAGS='-j1'
cmake --build . --config ${TRAVIS_BUILD_TYPE} --target test
cd ..











