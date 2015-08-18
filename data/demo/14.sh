#!/bin/bash
echo Generate a local drain direction map on basis of the digital
echo elevation map.
echo pcrcalc 'ldd.map=lddcreate(dem.map,1e31,1e31,1e31,1e31)'
pcrcalc 'ldd.map=lddcreate(dem.map,1e31,1e31,1e31,1e31)'