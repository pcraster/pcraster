import pcraster as pcr

pcr.setclone("dem.map")

# calculate a map with the distances to the nearest rainstation
raindist = pcr.spread("rainstat.map", pcr.scalar(0), pcr.scalar(1))
# writing map 'raindist' with filename 'raindist.map' to disk
pcr.report(raindist, "raindist.map")

# Calculate the infiltration capacity map by crossing the soil map
# and the infilcap.tbl
infilcap = pcr.lookupscalar("infilcap.tbl", "soil.map")
pcr.report(infilcap, "infilcap.map")

# Generate a local drain direction map on basis of the digital
# elevation map.
ldd = pcr.lddcreate("dem.map", 1e31, 1e31, 1e31, 1e31)
pcr.report(ldd, "ldd.map")


# Generating a map with a random value taken from a normal distribution
randomField = pcr.max(pcr.scalar(0), pcr.scalar(0.005) + pcr.mapnormal() / pcr.scalar(1000))
pcr.report(randomField, "randomField.map")

# Execute the accuthreshold operator with simulated rainfall
runoff = pcr.accuthresholdflux("ldd.map", randomField, "infilcap.map")
pcr.report(runoff, "runoff.map")

# Generating a map holding elevation values above 95m
uplandArea = pcr.ifthen("dem.map" > pcr.scalar(95), "dem.map")
pcr.report(uplandArea, "upland.map")
