#!/usr/bin/env bash
set -e


# We need to create an out of source build
cd $SRC_DIR

mkdir -p build

# In case find_package Python fails: force it
PYTHON=python
Python_INCLUDE_DIR="$(${PYTHON} -c 'import sysconfig; print(sysconfig.get_path("include"))')"
Python_NumPy_INCLUDE_DIR="$(${PYTHON} -c 'import numpy;print(numpy.get_include())')"
CMAKE_ARGS="${CMAKE_ARGS} -DPython_EXECUTABLE:PATH=${PYTHON}"
CMAKE_ARGS="${CMAKE_ARGS} -DPython_INCLUDE_DIR:PATH=${Python_INCLUDE_DIR}"
CMAKE_ARGS="${CMAKE_ARGS} -DPython_NumPy_INCLUDE_DIR:PATH=${Python_NumPy_INCLUDE_DIR}"

# NOTE
# NOTE Use -Werror ONLY in our development builds, never on conda-forge
# NOTE
# cmake -E env CFLAGS="-Werror=deprecated-declarations" CXXFLAGS="-Werror=deprecated-declarations" \
cmake -E env CFLAGS="${CFLAGS} -Werror=deprecated-declarations" CXXFLAGS="${CXXFLAGS} -D_LIBCPP_DISABLE_AVAILABILITY -Werror=deprecated-declarations" \
cmake ${CMAKE_ARGS} \
  -S $SRC_DIR \
  -B build \
  -G"Ninja" \
  -D CMAKE_BUILD_TYPE=Release \
  -D CMAKE_INSTALL_PREFIX="${PREFIX}" \
  -D PCRASTER_PYTHON_INSTALL_DIR=${SP_DIR} \
  -D PCRASTER_BUILD_TEST=ON

cmake --build build --target all --parallel ${CPU_COUNT}

ctest --test-dir build --output-on-failure --build-config Release

cmake --build build --target install
