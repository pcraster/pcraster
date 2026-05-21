#!/usr/bin/env bash

set -e


if [ $(uname) == Linux ]; then
  # Kindly guide to conda's OpenGL...
  PLATFORM_OPTIONS="" #-D OPENGL_opengl_LIBRARY:PATH=${BUILD_PREFIX}/${HOST}/sysroot/usr/lib64/libGL.so \
  #-D OPENGL_gl_LIBRARY:PATH=${BUILD_PREFIX}/${HOST}/sysroot/usr/lib64/libGL.so \
  #-D OPENGL_glu_LIBRARY:PATH=${PREFIX}/lib/libGLU.so"
else
  PLATFORM_OPTIONS="" #-D GDAL_INCLUDE_DIR=${PREFIX}/include"
  export TMPDIR=/tmp
fi


# We need to create an out of source build
cd $SRC_DIR

mkdir -p build && cd build



# In case find_package Python fails: force it
PYTHON=python
Python_INCLUDE_DIR="$(${PYTHON} -c 'import sysconfig; print(sysconfig.get_path("include"))')"
Python_NumPy_INCLUDE_DIR="$(${PYTHON} -c 'import numpy;print(numpy.get_include())')"
CMAKE_ARGS="${CMAKE_ARGS} -DPython3_EXECUTABLE:PATH=${PYTHON}"
CMAKE_ARGS="${CMAKE_ARGS} -DPython3_INCLUDE_DIR:PATH=${Python_INCLUDE_DIR}"
CMAKE_ARGS="${CMAKE_ARGS} -DPython3_NumPy_INCLUDE_DIR:PATH=${Python_NumPy_INCLUDE_DIR}"


# master branch
cmake ${CMAKE_ARGS} .. -G"Ninja" -DCMAKE_BUILD_TYPE=Release \
  -D CMAKE_INSTALL_PREFIX="${PREFIX}" \
  -D PCRASTER_PYTHON_INSTALL_DIR=${SP_DIR} \
  -D PCRASTER_WITH_FLAGS_NATIVE=OFF \
  -D PCRASTER_BUILD_TEST=ON \
  $PLATFORM_OPTIONS

cmake --build . --target all

export LOGNAME=pcrtester

ctest --output-on-failure

cmake --build . --target install
