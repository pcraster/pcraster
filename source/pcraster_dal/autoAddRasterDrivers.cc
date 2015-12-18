    autoAddDriver(new MemoryRasterDriver(&(library()->cacheDataPool())));
    autoAddDriver(new MemoryRasterDriver(&(library()->memoryDataPool())));
    autoAddDriver(new CSFRasterDriver());

    // Add all raster drivers supported by the current gdal installation.
    for(GDALRasterDriver::iterator it = GDALRasterDriver::begin();
           it != GDALRasterDriver::end(); ++it) {

      // We have our own for PCRaster (CSFRasterDriver).
      if(std::string((*it)->GetDescription()) != "PCRaster") {
        autoAddDriver(new GDALRasterDriver(*it));
      }
    }
