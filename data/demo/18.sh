#!/bin/bash
echo For timestep 18, calculate the actual infiltration with the
echo accuthresholdstate operator.
echo pcrcalc 'infil.map=accuthresholdstate(ldd.map,rainfall.018,infilcap.map)'
pcrcalc 'infil.map=accuthresholdstate(ldd.map,rainfall.018,infilcap.map)'