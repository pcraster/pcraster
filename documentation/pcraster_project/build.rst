Compiling PCRaster from source
==============================

.. note::

   The following build instructions apply to PCRaster version 4.3 (or later). You don't want to build earlier versions yourself. Really, you don't.




Requirements
------------

Building PCRaster depends on various software tools, development packages and libraries, at least:

  * Compiler supporting C11 and C++17 (gcc 7 or higher)
  * CMake >=3.14
  * git
  * Python > 3.5
  * Boost > 1.66
  * NumPy
  * GDAL >= 2.0
  * Qt > 5.7
  * XercesC 3.2

You will need to build libraries yourself in case they are not available on your system or those versions are too old.

Obtaining the sources
~~~~~~~~~~~~~~~~~~~~~

Preferably you should use a versioned software release to build PCRaster.
This is recommended in case you want to use PCRaster in production.
You can obtain a source package as follows:


.. code-block:: bash

   $ # sha256: 2772ae0dc228cebaf1891623526a097799ab7fe212c606f6d2fe6c3c47b70be6
   $ wget http://pcraster.geo.uu.nl/pcraster/packages/src/pcraster-4.3.0.tar.bz2


It is also possible to obtain the PCRaster sources from our code repository, e.g. in case you want to test the latest developments:


.. code-block:: bash

   $ mkdir pcraster_build
   $ cd pcraster_build
   $ git clone --recursive https://github.com/pcraster/pcraster.git



Configuring and building
~~~~~~~~~~~~~~~~~~~~~~~~

CMake requires an out of source build.
Make sure that you check and adapt the install location and Python version:

.. code-block:: bash

   $ mkdir build
   $ cd build

   $ cmake -G"Unix Makefiles" ../pcraster -DCMAKE_INSTALL_PREFIX=$HOME/pcraster-4.3.0 -DPython3_EXECUTABLE:FILEPATH=/usr/bin/python3.7

In case some of the 3rd party libraries are not installed in default locations, you need to provide hints before you call CMake.
For example, on CentOS 7 you may need to use:


.. code-block:: bash

   $ export BOOST_INCLUDEDIR=/usr/include/boost169/
   $ export BOOST_LIBRARYDIR=/usr/lib64/boost169/
   $ export GDAL_ROOT=/tmp/gdal-2.4.1
   $ export Qt5_DIR=/tmp/qt5.12.4

   # CMake command as above
   $ cmake -G"Unix Makefiles" ....

Before you build check the CMake output to ensure that correct library versions are used.



After a successful configure run, build and install PCRaster:


.. code-block:: bash

   $ NR_CPUS=4
   $ cmake --build . --target all -- -j$NR_CPUS
   $ cmake --build . --target install




Post-install
~~~~~~~~~~~~

After a successful build you will need to add $HOME/pcraster-4.3.0/bin to your PATH environment variable and $HOME/pcraster-4.3.0/python to your PYTHONPATH environment variable to run the PCRaster applications.


Build options
~~~~~~~~~~~~~

Some build options can be changed. In the build directory type:


.. code-block:: bash

   $ ccmake .


Toggle and browse to the PCRASTER entries. Modify entries with great care.
