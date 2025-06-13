@echo on

rem We need to create an out of source build
mkdir build

if errorlevel 1 exit 1

cd build

if errorlevel 1 exit 1


rem Ensure desired Boost version is selected by CMake
rem  set "BOOST_ROOT=%PREFIX%"
rem  set "BOOST_NO_SYSTEM_PATHS=ON"



where python

python -c 'import numpy;print(f"numpy_version={numpy.__version__}")'


cmake .. -G"Ninja" ^
-D CMAKE_BUILD_TYPE=Release  ^
-D CMAKE_INSTALL_PREFIX:PATH="%LIBRARY_PREFIX%" ^
-D PCRASTER_PYTHON_INSTALL_DIR="%SP_DIR%" ^
-D PCRASTER_BUILD_TEST=ON ^
-D CMAKE_C_COMPILER=cl ^
-D CMAKE_CXX_COMPILER=cl


if errorlevel 1 exit 1

cmake --build . --target all --parallel %CPU_COUNT%

if errorlevel 1 exit 1

cmake --build . --target install

if errorlevel 1 exit 1