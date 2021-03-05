# PCRaster
Environmental modelling software

PCRaster is a collection of tools and software libraries tailored to the construction of spatio-temporal environmental models. Application domains are amongst others hydrology (rainfall-runoff, global water balance, groundwater (with Modflow)), ecology, or land use change. Two scripting languages (Python and PCRcalc) include a rich set of spatial operations for manipulating and analysing raster maps. A Python framework supports Monte Carlo simulations and data assimilation (Ensemble Kalman Filter and Particle Filter). The Aguila tool allows for the interactive visualisation of stochastic spatio-temporal data.

You can find more information about our research and development projects on [our website.](http://computationalgeography.org/) Information on PCRaster is given at the [project website](http://www.pcraster.eu/), and online documentation can be found [here](http://pcraster.geo.uu.nl/support/documentation/). For questions regarding the usage of PCRaster please use our [mailing list](https://lists.geo.uu.nl/mailman/listinfo/pcraster-info), bugs can be reported via our [issue tracker](https://github.com/pcraster/pcraster/issues).


## Installation
[![Anaconda-Server Badge](https://anaconda.org/conda-forge/pcraster/badges/version.svg)](https://anaconda.org/conda-forge/pcraster)
[![Anaconda-Server Badge](https://anaconda.org/conda-forge/pcraster/badges/platforms.svg)](https://anaconda.org/conda-forge/pcraster)
[![Anaconda-Server Badge](https://anaconda.org/conda-forge/pcraster/badges/installer/conda.svg)](https://conda.anaconda.org/conda-forge)

Packages are available for Linux, macOS and Windows via [conda-forge](https://github.com/conda-forge/pcraster-feedstock).
Install PCRaster with:
```bash
conda install -c conda-forge pcraster
```


## Build status

| OS | Compilers | Status |
|----|-----------|--------|
| Linux, macOS | gcc-9, gcc-10, clang-11 | [![Linux build Status](https://travis-ci.org/pcraster/pcraster.svg?branch=master)](https://travis-ci.org/pcraster/pcraster) |
| Windows | vs-2017 | Checked manually |
