# PCRaster
Environmental modelling software

PCRaster is a collection of tools and software libraries tailored to the construction of spatio-temporal environmental models. Application domains are amongst others hydrology (rainfall-runoff, global water balance, groundwater (with Modflow)), ecology, or land use change. Two scripting languages (Python and PCRcalc) include a rich set of spatial operations for manipulating and analysing raster maps. A Python framework supports Monte Carlo simulations and data assimilation (Ensemble Kalman Filter and Particle Filter). The Aguila tool allows for the interactive visualisation of stochastic spatio-temporal data.

You can find more information about our research and development projects on [our website.](http://computationalgeography.org/) Information on PCRaster is given at the [project website](http://www.pcraster.eu/), and online documentation can be found [here](https://pcraster.geo.uu.nl/pcraster/latest/documentation/index.html). For questions regarding the usage of PCRaster please use our [mailing list](https://lists.geo.uu.nl/mailman/listinfo/pcraster-info), bugs can be reported via our [issue tracker](https://github.com/pcraster/pcraster/issues).

PCRaster is maintained by the [Computational Geography](https://www.computationalgeography.org/) group at [Utrecht University](https://www.uu.nl/en/research/department-of-physical-geography) in The Netherlands.


## Installation
[![Conda Version](https://img.shields.io/conda/vn/conda-forge/pcraster.svg)](https://anaconda.org/conda-forge/pcraster)
[![Conda Platforms](https://img.shields.io/conda/pn/conda-forge/pcraster.svg)](https://anaconda.org/conda-forge/pcraster)
[![Conda Downloads](https://img.shields.io/conda/dn/conda-forge/pcraster.svg)](https://anaconda.org/conda-forge/pcraster)

Packages are available for Linux, macOS and Windows via [conda-forge](https://github.com/conda-forge/pcraster-feedstock).
Install PCRaster e.g. with:

```bash
conda install -c conda-forge pcraster
```

More information on the installation of PCRaster is given in the [documentation](https://pcraster.geo.uu.nl/pcraster/latest/documentation/pcraster_project/install.html).

## Build status
CI builds of our current development version:

[![Linux build status](https://github.com/pcraster/pcraster/actions/workflows/dev-linux.yaml/badge.svg)](https://github.com/pcraster/pcraster/actions/workflows/dev-linux.yaml)
[![macOS build status](https://github.com/pcraster/pcraster/actions/workflows/dev-macos.yaml/badge.svg)](https://github.com/pcraster/pcraster/actions/workflows/dev-macos.yaml)
[![Windows build status](https://github.com/pcraster/pcraster/actions/workflows/dev-windows.yaml/badge.svg)](https://github.com/pcraster/pcraster/actions/workflows/dev-windows.yaml)
[![Conda build status](https://github.com/pcraster/pcraster/actions/workflows/dev-conda.yaml/badge.svg)](https://github.com/pcraster/pcraster/actions/workflows/dev-conda.yaml)
