file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/test_gdal_import_1.py
"import pcraster
from osgeo import gdal,ogr,osr
gdal.UseExceptions()
try:
    from qgis import core
except ImportError:
    pass

raster = gdal.OpenEx('abs_Expr.map')
t = raster.GetGeoTransform()
srs = osr.SpatialReference()
srs.ImportFromEPSG(4326)
pcraster.setclone(5, 4, 3, 2, 1)
")

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/test_gdal_import_2.py
"from osgeo import gdal,ogr,osr
gdal.UseExceptions()
try:
    from qgis import core
except ImportError:
    pass
import pcraster
raster = gdal.OpenEx('abs_Expr.map')
t = raster.GetGeoTransform()
srs = osr.SpatialReference()
srs.ImportFromEPSG(4326)
pcraster.setclone(5, 4, 3, 2, 1)
")

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/test_gdal_import_3.py
"import pcraster
from osgeo import gdal,ogr,osr
gdal.UseExceptions()
try:
    from qgis import not_existing
except ImportError:
    pass

raster = gdal.OpenEx('abs_Expr.map')
t = raster.GetGeoTransform()
srs = osr.SpatialReference()
srs.ImportFromEPSG(4326)
pcraster.setclone(5, 4, 3, 2, 1)
")
