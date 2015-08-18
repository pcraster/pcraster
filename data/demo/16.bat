@echo off
echo Execute the accuthreshold operator for timestep 18.
echo.
echo Press any key to execute:
echo.
echo pcrcalc runoff.map=accuthresholdflux(ldd.map,rainfall.018,infilcap.map)
pause > nul
echo.
@echo on
pcrcalc runoff.map=accuthresholdflux(ldd.map,rainfall.018,infilcap.map)