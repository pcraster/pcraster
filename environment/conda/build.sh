#!/usr/bin/env bash
set -e


# We need to create an out of source build
cd $SRC_DIR

mkdir -p build


# master branch
cmake ${CMAKE_ARGS} -S $SRC_DIR -B build \
  -G"Ninja" -DCMAKE_BUILD_TYPE=Release \
  -D CMAKE_INSTALL_PREFIX="${PREFIX}" \
  -D PCRASTER_PYTHON_INSTALL_DIR=${SP_DIR} \
  -D PCRASTER_WITH_FLAGS_NATIVE=OFF \
  -D PCRASTER_BUILD_TEST=ON


cmake --build build --target all --parallel ${CPU_COUNT}

ctest --test-dir build --output-on-failure --build-config Release

cmake --build build --target install
