#!/bin/bash
set -e
set -v


echo $CC
echo $CXX
python --version
python -c "import numpy as n; print(n.__version__); print(n.get_include());"

cd $TRAVIS_BUILD_DIR/build
cmake --build . --config ${TRAVIS_BUILD_TYPE} --target all -- -j2

cmake --build . --config ${TRAVIS_BUILD_TYPE} --target test -- -j2

cd ..











