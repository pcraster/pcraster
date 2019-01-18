#!/bin/bash
set -e
set -v

sudo add-apt-repository ppa:beineri/opt-qt-5.12.0-xenial -y
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable -y
sudo apt-get update -qq
