#!/bin/bash
echo Execute the accuthreshold operator for timestep 18.
echo pcrcalc 'runoff.map=accuthresholdflux(ldd.map,rainfall.018,infilcap.map)'
pcrcalc 'runoff.map=accuthresholdflux(ldd.map,rainfall.018,infilcap.map)'