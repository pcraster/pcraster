#!/usr/bin/env bash

set -e


if [ $(uname) == Linux ]; then
  # Kindly guide to conda's OpenGL...
  PLATFORM_OPTIONS="-D OPENGL_opengl_LIBRARY:PATH=${BUILD_PREFIX}/${HOST}/sysroot/usr/lib64/libGL.so \
  -D OPENGL_gl_LIBRARY:PATH=${BUILD_PREFIX}/${HOST}/sysroot/usr/lib64/libGL.so \
  -D OPENGL_glu_LIBRARY:PATH=${PREFIX}/lib/libGLU.so"
else
  PLATFORM_OPTIONS="-D GDAL_INCLUDE_DIR=${PREFIX}/include"
  export TMPDIR=/tmp
fi


# We need to create an out of source build
cd $SRC_DIR

mkdir -p build


# master branch
cmake ${CMAKE_ARGS} -S $SRC_DIR -B build \
  -G"Ninja" -DCMAKE_BUILD_TYPE=Release \
  -D CMAKE_INSTALL_PREFIX="${PREFIX}" \
  -D PCRASTER_PYTHON_INSTALL_DIR=${SP_DIR} \
  -D PCRASTER_WITH_FLAGS_NATIVE=OFF \
  -D PCRASTER_BUILD_TEST=ON \
  -D PCRASTER_WITH_QT6=ON \
  $PLATFORM_OPTIONS

cmake --build build --target all

export LOGNAME=pcrtester

ctest --parallel 2 --test-dir build --output-on-failure --build-config Release

cmake --build build --target install
