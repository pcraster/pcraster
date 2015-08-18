@echo off
echo For timestep 18, calculate the actual infiltration with the 
echo accuthresholdstate operator.
echo.
echo Press any key to execute:
echo.
echo pcrcalc infil.map=accuthresholdstate(ldd.map,rainfall.018,infilcap.map)
pause > nul
echo.
@echo on
pcrcalc infil.map=accuthresholdstate(ldd.map,rainfall.018,infilcap.map)