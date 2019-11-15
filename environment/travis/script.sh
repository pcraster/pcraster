#!/bin/bash
set -e
set -v


cd $TRAVIS_BUILD_DIR/build
cmake --build . --config ${TRAVIS_BUILD_TYPE} -DCMAKE_RULE_MESSAGES=OFF --target all -- -j2

cmake --build . --config ${TRAVIS_BUILD_TYPE} --target test -- -j2

cd ..











