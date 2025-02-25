Changes
=======

.. PCRaster 4.5.0
.. --------------
..
.. The most relevant updates for users:
.. * aguila contour
..
.. We further improved the code quality and the build system to ensure an ongoing creation of PCRaster packages, amongst others:
..
.. * Modernising the source code. C11 and C++20 are now the default standards when compiling PCRaster.
.. * Various fixes raised by compiler warnings.
.. * Replacing numerous 3rd party code with C++ standard equivalents: numbers, gcd, tuple, format
.. * Qt6 by default 5 as option
.. * Including outdated Boost headers were removed
.. * find package config Boost
.. * Qt deprecated
.. * Fixed failing unit tests when building with EasyBuild
.. * prefixed modflow executable
.. * gcc13 minimum


PCRaster 4.4.1
--------------

This is a bug fix release for 4.4.

* We fixed a segmentation fault occurring at the end of running a `QGIS qgis_process <https://docs.qgis.org/latest/en/docs/user_manual/processing/standalone.html>`_ from the command line (`#378 <https://github.com/pcraster/pcraster/issues/378>`_, `#55 <https://github.com/jvdkwast/qgis-processing-pcraster/issues/55>`_).
* We fixed an incomplete conversion from numpy.nan to scalar missing values (`#140 <https://github.com/pcraster/pcraster/issues/140>`_).
* We removed a deprecated mode when opening files in the Arrayed Variables module.

For a list of solved issues see our `4.4.1 <https://github.com/pcraster/pcraster/milestone/12>`_ milestone.

PCRaster 4.4.0
--------------

PCRaster now supports the ARM architecture (`#341 <https://github.com/pcraster/pcraster/issues/341>`_), packages for Apple M1 are available on `conda-forge <https://github.com/conda-forge/pcraster-feedstock>`_.

Other updates relevant for users:

* You can compile PCRaster on your HPC with `EasyBuild <https://easybuild.io/>`_. Use or adapt our `easyconfig <https://github.com/pcraster/pcraster/tree/master/environment/configuration/easybuild>`_ files.
* The sources can be compiled on aarch64 Linux systems as well.
* Aguila was again refactored to reduce runtime library dependencies, notably the command line interface. The functionality should remain the same. In case you experience any differences to previous Aguila versions consider it as a defect and please report it at our `issues <https://github.com/pcraster/pcraster/issues/>`_ page.
* We fixed an issue where Aguila could hang on Windows (`#333 <https://github.com/pcraster/pcraster/issues/333>`_).
* We fixed an incorrect image plotting of LDD raster (`#362 <https://github.com/pcraster/pcraster/issues/362>`_).
* We fixed another segmentation fault at script exit when using modules from PCRaster, GDAL and QGIS (`#361 <https://github.com/pcraster/pcraster/issues/361>`_).

We further improved the code quality and the build system to ensure an ongoing creation of PCRaster packages, amongst others:

* Building against the latest Python 3 version. Supported are 3.8 - 3.11
* Replacing Boost.Filesystem by std::filesystem
* Boost is no longer a runtime dependency
* Further modernisation of the source code and build system.
* Various improvements to support gcc-12, clang-14, Visual Studio 2019 and 2022.

For a list of solved issues see our `4.4 <https://github.com/pcraster/pcraster/milestone/7>`_ milestone.


PCRaster 4.3.3
--------------

This is a bug fix release for 4.3.

* We improved the internal precision of the ``maptotal`` operation resulting in more reliable results when using large datasets (`#336 <https://github.com/pcraster/pcraster/issues/336>`_).
* We fixed several memory leaks in the ``multicore`` module (`#344 <https://github.com/pcraster/pcraster/issues/344>`_).
* We fixed a segmentation fault at module exit when PCRaster and GDAL are used (`#343 <https://github.com/pcraster/pcraster/issues/343>`_).


PCRaster 4.3.2
--------------

This is a bug fix release for 4.3.

* ``pcr.setclone``, ``pcr.readmap``: improved error message for non-PCRaster file arguments (`#337 <https://github.com/pcraster/pcraster/issues/337>`_).
* The ``mldd`` package is again included by default (`#235 <https://github.com/pcraster/pcraster/issues/235>`_).
* Several small fixes to allow building with newer versions of 3rd party libraries (`#339 <https://github.com/pcraster/pcraster/issues/339>`_).

For a list of solved issues see our `4.3.2 <https://github.com/pcraster/pcraster/milestone/9>`_ milestone.

PCRaster 4.3.1
--------------

This is a bug fix release for 4.3. We fixed:

* Aguila: incorrect x-axis display for timeseries and less than three timesteps (`#303 <https://github.com/pcraster/pcraster/issues/303>`_).
* Aguila: unhandled exception thrown instead of error messages on macOS (`#302 <https://github.com/pcraster/pcraster/issues/302>`_).
* ``pcr.aguila``: incorrect handling of function arguments (`#325 <https://github.com/pcraster/pcraster/issues/325>`_).
* An initialisation error when paths in the environment variable PATH do not exist (`#134 <https://github.com/pcraster/pcraster/issues/134>`_).
* Error checking for numpy2pcr (`#317 <https://github.com/pcraster/pcraster/issues/317>`_).
* Redundant error output in PCRaster Modflow (`#321 <https://github.com/pcraster/pcraster/issues/321>`_).
* Ignoring small well pumping rates (close to 0) in PCRaster Modflow (`#324 <https://github.com/pcraster/pcraster/issues/324>`_).

For a list of solved issues see our `4.3.1 <https://github.com/pcraster/pcraster/milestone/8>`_ milestone.

PCRaster 4.3.0
--------------

The most relevant updates for users:

* PCRaster is available on |condaforgefeedstocksurl| and can be installed using conda. Supported platforms are Linux, macOS and Windows. Supported Python versions are 3.6 and 3.7.
* We no longer provide support for Python 2.
* We fixed a bug occurring when pickling PCRaster maps. Note that PCRaster maps pickled with previous versions cannot be opened with this version.
* We fixed an incorrect return value while using a nonspatial condition in ``ifthen``.
* We fixed several minor inconsistencies in the ``multicore`` module.
* We added the Python functions ``cellvalue_by_coordinates``, ``cellvalue_by_index``, ``cellvalue_by_indices`` and ``version_tuple``.
* We added a ``plot`` function to create basic plots of PCRaster maps in case the matplotlib module is installed.
* On Windows, ``mapattr`` and ``legend`` show the menu again.
* We fixed the incorrect rendering of directional rasters in Aguila.
* PCRaster Modflow now supports the GHB package.
* PCRaster Modflow BCF package received an optional flag to directly pass hcond and vcond values to Modflow.
* Command line applications now show version numbers instead of a build date.
* The exit value of applications only showing the usage information changed from 1 (EXIT_FAILURE) to 0 (EXIT_SUCCESS).


Aguila was refactored to simplify the build process. The functionality remains the same. In case you experience any differences to previous Aguila versions consider it as a defect and please report it at our `issues <https://github.com/pcraster/pcraster/issues/>`_ page.

We further improved the code quality and the build system to ensure an ongoing creation of PCRaster packages, amongst others:

* Simplified building against multiple Python 3 versions (3.6, 3.7, 3.8)
* Replacing Boost.Python with Pybind11
* Replacing Qwt with QtCharts
* Modernising C++ code. C11 and C++17 are now the default standards when compiling PCRaster.
* Various improvements to support gcc-9, clang-7, Visual Studio 2017 and Python 3.8
* Reducing amount of dependencies
* You can now set a path to specify the install directory of the Python files. You can enable this with ``-DPCRASTER_PYTHON_INSTALL_DIR=<path>``, default is ``python``.
* We added configure options to include or exclude certain parts of the PCRaster software. You can, for example, disable building Aguila with ``-DPCRASTER_BUILD_AGUILA=OFF``.
* Specifically for Linux:

  - RPATH settings fixed for Python modules
  - Shared libraries now receive version numbers and sonames
  - The build type 'Release' now sets flags ``-march=native -mtune=native`` by default. You can disable this with ``-DPCRASTER_WITH_FLAGS_NATIVE=OFF``.
  - Interprocedural optimisation can be used in case supported by the compiler. You can enable this with ``-DPCRASTER_WITH_FLAGS_IPO=ON``.

PCRaster 4.3 is known to build with gcc (versions 7 to 10), clang (version 6 to 9) or msvc (2015 and 2017).
We recommend to no longer use gcc version 5 or 6 for building PCRaster.

For a more detailed list of solved issues see our `4.3 <https://github.com/pcraster/pcraster/milestone/6>`_ milestone.


.. |condaforgefeedstocksurl| raw:: html

   <a href="https://conda-forge.org/feedstock-outputs/index.html" target="_blank">conda-forge</a>

PCRaster 4.2.1
--------------
This is a bug fix release for 4.2. We fixed a memory leak in ``pcr2numpy`` and improved the error reporting at module import (`#233 <https://github.com/pcraster/pcraster/issues/233>`_, `#232 <https://github.com/pcraster/pcraster/issues/232>`_).


PCRaster 4.2.0
--------------

The most relevant updates for users:

* Python 3 is now the default version on Windows
* The multicore module was added to PCRaster Python
* On Windows, files larger than 2GB can be processed now
* Handling of non-spatials and pcr2numpy was fixed in the PCRaster Python module
* PCRaster Modflow now uses Modflow 2005
* The documentation pages of PCRaster projects were unified into a single manual

We also took measures to improve the code quality and the build system to ensure an ongoing creation of PCRaster packages, amongst others:

* Porting from Qt4 to Qt5
* Updating various dependencies (external libraries)
* Reducing amount of dependencies
* Building against dependencies installed by a package manager
* Removing obsolete scripts and configuration files
* Modernising CMake scripts
* Modernising C and C++ code towards C11 and C++11
* Various fixes raised by compiler warnings
* Various improvements to support Visual Studio 2015
* Various improvements to support clang and macOS


PCRaster 4.1.0
--------------
* On Windows, shared libraries are now installed in a seperate directory called ``lib``, just like on the other platforms. The path to this directory does not have to be listed in the ``$PATH`` environment variable. This has the advantage that the shared libraries shipped with PCRaster will never conflict with shared libraries shipped with other software, and visa versa.

Aguila
^^^^^^
It is now possible again to select a specific column from a timeseries file, like it used to (`Ticket #635 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/635/>`_)

PCRaster Python package
^^^^^^^^^^^^^^^^^^^^^^^
* PCRaster data types can now be serialized by the pickle module (`Ticket #593 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/593/>`_). See also the ``pickle`` Python module. More about this in a future release.

GDAL
^^^^
* The submitted patches for the GDAL PCRaster driver support the dynamic creation of PCRaster maps (`Ticket #664 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/664/>`_) and contain some updates (`Ticket #679 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/679/>`_). We expect the functionality to be integrated in one of the next official GDAL releases.

Documentation
^^^^^^^^^^^^^
* Minor updates to documentation of ``spatial``, ``transient`` and ``horizontan`` operations (`Ticket #620 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/620/>`_, `Ticket #677 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/677/>`_, `Ticket #678 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/678/>`_)


PCRaster 4.0.2
--------------

This release improves the reliability of PCRaster applications relating to large data sets, includes functional enhancements for the Modflow extension, and fixes several bugs.


PCRaster model engine
^^^^^^^^^^^^^^^^^^^^^
* An erroneous check was replaced to allow for processing maps with more than 2^31 - 1 cells on 64bit systems (`Ticket #648 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/648/>`_)

col2map
^^^^^^^
* We fixed the incorrect allocation of cells when using maps with more than 2^31 - 1 cells (`Ticket #661 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/661/>`_)
* We fixed the incorrect printout of cells with more than one record (`Ticket #660 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/660/>`_)


Modflow extension
^^^^^^^^^^^^^^^^^

*  Added functions to obtain cell-by-cell values for storage, constant heads, and front/right/lower flow face (`Ticket #643 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/643/>`_)
*  Fixed incorrect reporting of the Python getRiverLeakage (`Ticket #663 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/663/>`_)
*  Fixed activation of well package for time steps > 1 (`Ticket #658 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/658/>`_)
*  Fixed setting of rewetting thresholds for top layer type laycon 1 (`Ticket #657 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/657/>`_)
*  Fixed uѕage of reserved unit numbers for Modflow input files (`Ticket #662 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/662/>`_)
*  Maps now can be reported at time steps where Modflow fails to converge (`Ticket #669 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/669/>`_)
*  Minor performance improvements reducing the memory and I/O load

Aguila
^^^^^^
* We fixed the incorrect colour assignment of 2D directional data types (`Ticket #670 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/670/>`_)
* We fixed the incorrect colour assignment of 3D directional and scalar data types (`Ticket #641 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/641/>`_)

Documentation
^^^^^^^^^^^^^
* Minor updates (`Ticket #628 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/628/>`_, `Ticket #659 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/659/>`_)


PCRaster 4.0.1
--------------
This is a bug fix release for 4.0.

Global options ``chezy`` and ``manning`` for dynwavestate, dynwaveflux, dynamicwave (pcrcalc, PCRaster Python package)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
We discovered a documentation error for the operations dynwavestate, dynwaveflux and dynamicwave.
The manual stated that the Chezy algorithm was the default algorithm to calculate the dynamic flow equation.
In fact, it was calculated by the Manning algorithm by default.

If you did not use any global option, your results were calculated by the Manning equation. From now on, without specifying global options, results will be calculated by the Manning equation as well.

If you used either ``chezy`` or ``manning`` as global option, the corresponding algorithms were used. This behaviour remains unchanged.

To obtain values calculated with the Chezy algorithm, you now need to specify explicitly either
``--chezy`` on the command line, ``#! --chezy`` in PCRcalc scripts, or ``setglobaloption("chezy")`` in Python scripts.

dynamicwave (pcrcalc, PCRaster Python package)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
We discovered and fixed a bug in the dynamicwave operation while using the Manning algorithm (`Ticket #609 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/609/>`_).
As the Manning algorithm was used as default (see the remarks above) it is expected that your model results will change.

Aguila
^^^^^^
* Fixed '#624 cannot open .tss files in PCRASTER 4 aguila version' (`Ticket #624 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/624/>`_)

PCRaster Python package
^^^^^^^^^^^^^^^^^^^^^^^
* Fixed a wrong number of arguments in the base class for dynamic models (`Ticket #603 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/603/>`_)

resample
^^^^^^^^
* Fixed a regression that caused the generation of MV in all cells while using the crop option (`Ticket #485 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/485/>`_)

pcrcalc
^^^^^^^
* Fixed a redundant timestep output (`Ticket #625 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/625/>`_)

Documentation
^^^^^^^^^^^^^
* The manual pages include updates for the mapattr application and the lookupstate and lookuppotential operations (`Ticket #613 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/613/>`_, `Ticket #601 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/601/>`_)

Developer information
^^^^^^^^^^^^^^^^^^^^^
* Ported machine_status.py to newer apt_pkg, updated list of required applications for compiling PCRaster (`Ticket #610 <https://sourceforge.net/p/pcraster/bugs-and-feature-requests/610/>`_)



PCRaster 4.0.0
--------------
General
^^^^^^^
* Changed the license of all PCRaster source code to the `GPLv3 <http://www.gnu.org/licenses/gpl-3.0.html>`_ open source license. Moved all sourcecode to the `PCRaster Open Source Tools site <https://sourceforge.net/projects/pcraster/>`_ at SourceForge.
* The installation process of PCRaster has been simplified. On all platforms we distribute a zip file which can be unzipped at a preferred location. After setting two environment variables, PCRaster is ready to be used. The goal is to make it possible to install multiple versions of PCRaster at the same time. This has the advantage that older models can still be run with older installed versions of PCRaster. And it allows us to keep improving PCRaster, even if we break backwards compatibility (we prefer not to, but sometimes there is a good reason).
* Removed support for reading HDF4 formatted rasters. Maintaining support for this format proved to be too much of a hassle.

pcrcalc
^^^^^^^
* Removed support for encrypting models.
* Removed support for license specific functionality (like missing value compression). All features that used to require a commercial license are available for everybody now.

resample
^^^^^^^^
* Fixed the spurious creation of adjacent raster cells while using resample as cookie cutter (`Ticket #463 <http://sourceforge.net/p/pcraster/bugs-and-feature-requests/463/>`_)

PCRaster Python package
^^^^^^^^^^^^^^^^^^^^^^^
* Updated the code to allow the garbage collector to reclaim memory used by some of the framework class instanceѕ, after the last reference goes out of scope.
* Updated the code to prevent that the memory used by the PCRaster Python extension increases during a model run.
* PCRaster Python package now depends on Python 2.7.
* PCRaster Python package uses lower case names for package names. Update all PCRaster related imports and change them to lower case. See also the `Style Guide for Python Code <http://www.python.org/dev/peps/pep-0008/>`_.
* Removed ``pcraster.numpy`` sub-package. Numpy functionality is merged in the ``pcraster`` main package and available without an explicit import of the ``numpy`` sub-package. Remove any import of ``pcraster.numpy`` and rename any calls of ``pcraster.numpy.pcr2numpy`` and ``pcraster.numpy.numpy2pcr`` to ``pcraster.pcr2numpy`` and ``pcraster.numpy2pcr``.
* Removed ``pcr2numarray`` and ``numarray2pcr`` which were already deprecated. Use ``pcr2numpy`` and ``numpy2pcr``.
* Reimplemented ``numpy2pcr``. It is faster now.
* Added a `setclone` overload taking `nrRows`, `nrCols`, `cellSize`, `west`, `north`. No need to pass the name of an existing raster anymore.

MODFLOW extension
^^^^^^^^^^^^^^^^^
* Fixed a crash.
* Renamed extension from ``PCRasterModflow`` to ``pcraster_modflow``.
