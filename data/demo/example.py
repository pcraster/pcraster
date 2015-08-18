from pcraster import *

setclone("dem.map")

# calculate a map with the distances to the nearest rainstation
raindist = spread("rainstat.map", scalar(0), scalar(1))
# writing map 'raindist' with filename 'raindist.map' to disk
report(raindist, "raindist.map")

# Calculate the infiltration capacity map by crossing the soil map
# and the infilcap.tbl
infilcap = lookupscalar("infilcap.tbl", "soil.map")
report(infilcap, "infilcap.map")

# Generate a local drain direction map on basis of the digital
# elevation map.
ldd = lddcreate("dem.map", 1e31, 1e31, 1e31, 1e31)
report(ldd, "ldd.map")


# Generating a map with a random value taken from a normal distribution
randomField = max(scalar(0), scalar(0.005) + mapnormal() / scalar(1000))
report(randomField, "randomField.map")

# Execute the accuthreshold operator with simulated rainfall
runoff = accuthresholdflux("ldd.map", randomField, "infilcap.map")
report(runoff, "runoff.map")

# Generating a map holding elevation values above 95m
uplandArea = ifthen("dem.map" > scalar(95), "dem.map")
report(uplandArea, "upland.map")
