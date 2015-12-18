// Add all feature drivers supported by the current gdal installation.
auto* manager = GetGDALDriverManager();

for(int i = 0; i < manager->GetDriverCount(); ++i) {
  auto* driver = manager->GetDriver(i);
  auto metadata = driver->GetMetadata();

  if(CSLFetchBoolean(metadata, GDAL_DCAP_VECTOR, FALSE)) {
    autoAddDriver(new OgrFeatureDriver(driver));
  }
}
