@echo off
echo Execute the rainfall simulation model rain.mod.
echo.
echo Press any key to execute:
echo.
echo pcrcalc -f rain.mod
pause > nul
echo.
@echo on
pcrcalc -f rain.mod