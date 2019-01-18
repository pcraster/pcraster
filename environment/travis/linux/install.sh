# Source this script to keep settings valid for other scripts!
#!/bin/bash
set -e
set -v

eval "${MATRIX_EVAL}"

cwdir=${PWD}

sudo apt-get install qt512base
export PATH=/opt/qt512/bin:$PATH

sudo apt-get install libqwt-qt5-dev

sudo apt-get install gdal-bin libgdal-dev libxerces-c-dev libxml2-dev libxslt1-dev libboost-all-dev libncurses5-dev libpython-dev libxml2 libxml2-utils mesa-common-dev libglu1-mesa-dev libgl1-mesa-glx cmake

cd $TRAVIS_BUILD_DIR
pip install --user --upgrade pip
pip install --user --upgrade numpy
pip install --user --upgrade docopt
pip install --user --upgrade sphinx
pip install --user --upgrade psutil

# Return to initial directory
cd ${cwdir}
