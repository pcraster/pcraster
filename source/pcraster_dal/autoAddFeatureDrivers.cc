// Add all drivers supported by the current gdal installation.
OGRSFDriverRegistrar& manager(*OGRSFDriverRegistrar::GetRegistrar());

for(int i = 0; i < manager.GetDriverCount(); ++i) {
  autoAddDriver(new OgrFeatureDriver(manager.GetDriver(i)));
}
