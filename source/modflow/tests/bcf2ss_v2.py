# bcf2ss
from pcraster import *

nrRows = 10
nrCols = 15
cellSize = 500.0
west = 0.0
north = 0.0

setclone(nrRows, nrCols, cellSize, west, north)
bcf2ss = initialise(clone())

bcf2ss.createBottomLayer(spatial(scalar(-50.0)), spatial(scalar(0.0)))
bcf2ss.addConfinedLayer(spatial(scalar(50.0)))
bcf2ss.addLayer(spatial(scalar(150.0)))

bcf2ss.setBoundary("ib_1.pcrmap", 1)
bcf2ss.setBoundary("ib_3.pcrmap", 3)
bcf2ss.setInitialHead(spatial(scalar(0.0)), 1)
bcf2ss.setInitialHead(spatial(scalar(0.0)), 3)

calc = False
bcf2ss.setConductivity(0, spatial(scalar(500.0)), spatial(scalar(0.003)), 1, calc)
bcf2ss.setConductivity(0, spatial(scalar(0.0)), spatial(scalar(0.000)), 2, calc)
bcf2ss.setConductivity(1, spatial(scalar(10.0)), spatial(scalar(0.001)), 3, calc)
bcf2ss.setDryHead(777.77)
bcf2ss.setNoFlowHead(999.99)

bcf2ss.setWettingParameter(1, 1, 0)
bcf2ss.setWetting("bcf2ss_wet.map", 3)

bcf2ss.setRecharge(spatial(scalar(0.004)), 3)

bcf2ss.setRiver("rh.pcrmap", "rb.pcrmap", "rc.pcrmap", 1)

bcf2ss.setDISParameter(4, 0, 1, 1, 1, 1)

bcf2ss.setPCG(40, 20, 1, 0.001, 1000.0, 1, 2, 1)

# time step 1
# execute in subdirectory 1
bcf2ss.run("1")
report(bcf2ss.getHeads(1), "hTwo_1.map")
report(bcf2ss.getHeads(3), "hOne_1.map")
report(bcf2ss.getRecharge(1), "rTwo_1.map")
report(bcf2ss.getRecharge(3), "rOne_1.map")
report(bcf2ss.getRiverLeakage(1), "riTwo_1.map")
report(bcf2ss.getRiverLeakage(3), "riOne_1.map")
report(bcf2ss.getConstantHead(1), "chTwo_1.map")
report(bcf2ss.getConstantHead(3), "chOne_1.map")
report(bcf2ss.getFrontFace(1), "ffTwo_1.map")
report(bcf2ss.getFrontFace(3), "ffOne_1.map")
report(bcf2ss.getRightFace(1), "rfTwo_1.map")
report(bcf2ss.getRightFace(3), "rfOne_1.map")
report(bcf2ss.getLowerFace(3), "lfOne_1.map")

# time step 2
bcf2ss.setWell("bcf2ss_wel.map", 1)
bcf2ss._set_run_command("mf2005", "pcrmf.nam")
bcf2ss.run("1")
report(bcf2ss.getHeads(1), "hTwo_2.map")
report(bcf2ss.getHeads(3), "hOne_2.map")
report(bcf2ss.getRecharge(1), "rTwo_2.map")
report(bcf2ss.getRecharge(3), "rOne_2.map")
report(bcf2ss.getRiverLeakage(1), "riTwo_2.map")
report(bcf2ss.getRiverLeakage(3), "riOne_2.map")
report(bcf2ss.getConstantHead(1), "chTwo_2.map")
report(bcf2ss.getConstantHead(3), "chOne_2.map")
report(bcf2ss.getFrontFace(1), "ffTwo_2.map")
report(bcf2ss.getFrontFace(3), "ffOne_2.map")
report(bcf2ss.getRightFace(1), "rfTwo_2.map")
report(bcf2ss.getRightFace(3), "rfOne_2.map")
report(bcf2ss.getLowerFace(3), "lfOne_2.map")
