#!/bin/bash
echo Calculate the infiltration capacity map by crossing the soil map
echo and the infilcap.tbl
echo pcrcalc 'infilcap.map=lookupscalar(infilcap.tbl,soil.map)'
pcrcalc 'infilcap.map=lookupscalar(infilcap.tbl,soil.map)'