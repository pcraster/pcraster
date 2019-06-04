#!/bin/bash
set -e
set -v

sudo add-apt-repository ppa:beineri/opt-qt-5.12.0-xenial -y
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable -y
sudo apt-get update -qq

# Update CMake.
wget --no-check-certificate http://www.cmake.org/files/v3.14/cmake-3.14.4-Linux-x86_64.tar.gz
