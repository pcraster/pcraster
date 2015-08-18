@echo off
echo Calculate the infiltration capacity map by crossing the soil map 
echo and the infilcap.tbl
echo.
echo Press any key to execute:
echo.
echo pcrcalc infilcap.map=lookupscalar(infilcap.tbl,soil.map)
pause > nul
echo.
@echo on
pcrcalc infilcap.map=lookupscalar(infilcap.tbl,soil.map)