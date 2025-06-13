@echo on

rem We need to create an out of source build
mkdir build

if errorlevel 1 exit 1

cd build

if errorlevel 1 exit 1


rem Ensure desired Boost version is selected by CMake
rem  set "BOOST_ROOT=%PREFIX%"
rem  set "BOOST_NO_SYSTEM_PATHS=ON"


cmake .. -G"Ninja" ^
-D CMAKE_BUILD_TYPE=Release  ^
-D CMAKE_PREFIX_PATH="%LIBRARY_PREFIX%;%CMAKE_PREFIX_PATH%" ^
-D CMAKE_INSTALL_PREFIX:PATH="%LIBRARY_PREFIX%" ^
-D Python3_ROOT_DIR:PATH=%LIBRARY_PREFIX% ^
-D Python3_EXECUTABLE="%PYTHON%" ^
-D PYTHON_EXECUTABLE="%PYTHON%" ^
-D Python3_ROOT_DIR="%PREFIX%" ^
-D PCRASTER_BUILD_TEST=OFF ^
-D PCRASTER_PYTHON_INSTALL_DIR="%SP_DIR%"

if errorlevel 1 exit 1

cmake --build . --target all --parallel %CPU_COUNT%

if errorlevel 1 exit 1

cmake --build . --target install

if errorlevel 1 exit 1