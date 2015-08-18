#!/bin/bash
echo Calculate a map with the distances to the nearest rainstation and display
echo it with the rainstations map.
echo pcrcalc 'raindist.map=spread(rainstat.map,0,1)'
echo aguila raindist.map rainstat.map
pcrcalc 'raindist.map=spread(rainstat.map,0,1)'
aguila raindist.map rainstat.map