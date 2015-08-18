@echo off
echo Calculate a map with the unique cell value of the 
echo nearest rainstation and display it.
echo.
echo Press any key to execute:
echo.
echo  pcrcalc rainzone.map=spreadzone(rainstat.map,0,1) 
echo aguila rainzone.map rainstat.map raindist.map
pause > nul
echo.
@echo on
pcrcalc rainzone.map=spreadzone(rainstat.map,0,1) 
aguila rainzone.map rainstat.map raindist.map