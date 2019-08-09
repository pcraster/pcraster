# Source this script to keep settings valid for other scripts!
#!/bin/bash
set -e
set -v

eval "${MATRIX_EVAL}"

cwdir=${PWD}

sudo apt-get install qt512base qt512charts-no-lgpl
export PATH=/opt/qt512/bin:$PATH

sudo apt-get install gdal-bin libgdal-dev libxerces-c-dev libxml2-dev libxslt1-dev libboost-all-dev libncurses5-dev  libxml2 libxml2-utils mesa-common-dev libglu1-mesa-dev libgl1-mesa-glx cmake

sudo apt-get install python3 libpython3-dev python3-numpy python3-psutil

mkdir $TRAVIS_BUILD_DIR/local

cd $TRAVIS_BUILD_DIR/local
tar zxf ../cmake-3.14.4-Linux-x86_64.tar.gz
export PATH=$TRAVIS_BUILD_DIR/local/cmake-3.14.4-Linux-x86_64/bin:$PATH

# Return to initial directory
cd ${cwdir}
