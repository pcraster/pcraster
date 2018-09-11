#!/bin/bash
set -e
set -v

sudo add-apt-repository ppa:beineri/opt-qt562-trusty -y
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable -y
sudo apt-get update -qq

# Update CMake.
wget --no-check-certificate http://www.cmake.org/files/v3.12/cmake-3.12.1-Linux-x86_64.tar.gz

# Travis Qwt 6.0 does not cooperate with Qt5
# We build and install those ourselves using Peacock.
git clone --recursive https://github.com/geoneric/peacock.git
