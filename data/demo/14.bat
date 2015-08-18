@echo off
echo Generate a local drain direction map on basis of the digital 
echo elevation map.
echo.
echo Press any key to execute:
echo.
echo pcrcalc ldd.map=lddcreate(dem.map,1e31,1e31,1e31,1e31)
pause > nul
echo.
@echo on
pcrcalc ldd.map=lddcreate(dem.map,1e31,1e31,1e31,1e31)