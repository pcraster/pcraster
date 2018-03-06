# This example script demonstrates the use of the
# PCRasterModflow extension. It recreates the example
# problem given in the user guide of MODFLOW-2000
# (Open file report 00-92)
#
# To run the script use
# python example.py

from pcraster import *


setclone("clone.map")
mf = initialise(clone())

# grid specification
mf.createBottomLayer("bottom.map", "l1_top.map")
mf.addConfinedLayer("l2_top.map")
mf.addLayer("l3_top.map")
mf.addConfinedLayer("l4_top.map")
mf.addLayer("elev.map")

# simulation parameter
mf.setDISParameter(1, 0, 86400, 1, 1, 1)

# adding the boundary conditions to the MODFLOW model
mf.setBoundary("l1_bound.map", 1)
mf.setBoundary("l3_bound.map", 3)
mf.setBoundary("l5_bound.map", 5)

# adding the initial values to the MODFLOW model
mf.setInitialHead("head.map", 1)
mf.setInitialHead("head.map", 3)
mf.setInitialHead("head.map", 5)

# adding the conductivities to the MODFLOW model
mf.setConductivity(0, "hcond_l1.map", "vcond_l1.map", 1)
mf.setConductivity(0, "hcond_l1.map", "vcond_l2.map", 2)
mf.setConductivity(0, "hcond_l3.map", "vcond_l3.map", 3)
mf.setConductivity(0, "hcond_l3.map", "vcond_l4.map", 4)
mf.setConductivity(1, "hcond_l5.map", "vcond_l5.map", 5)

# solver package
mf.setSIP(50, 5, 1.0, 0.001, 0, 0.001)

mf.setDryHead(1E30)

# adding the drain
mf.setDrain("drain_elev.map", "drain_cond.map", 5)

# specifying the recharge
mf.setRecharge("rch.map", 1)

# adding well
mf.setWell("well_l1.map", 1)
mf.setWell("well_l3.map", 3)
mf.setWell("well_l5.map", 5)

# execute MODFLOW modelling engine
mf.run()

# retrieve head values
hFive = mf.getHeads(5)
report(hFive, "hFive.map")
hThree = mf.getHeads(3)
report(hThree, "hThree.map")
hOne = mf.getHeads(1)
report(hOne, "hOne.map")

# retrieve drain output
dFive = mf.getDrain(5)
report(dFive, "dFive.map")

# retrieve recharge output
rFive = mf.getRecharge(5)
report(rFive, "rFive.map")

# retrieve BCF outputs
c5 = mf.getConstantHead(5)
report(c5, "c5.map")
c3 = mf.getConstantHead(3)
report(c3, "c3.map")
c1 = mf.getConstantHead(1)
report(c1, "c1.map")

fr5 = mf.getRightFace(5)
report(fr5, "fr5.map")
fr3 = mf.getRightFace(3)
report(fr3, "fr3.map")
fr1 = mf.getRightFace(1)
report(fr1, "fr1.map")

ff5 = mf.getFrontFace(5)
report(ff5, "ff5.map")
ff3 = mf.getFrontFace(3)
report(ff3, "ff3.map")
ff1 = mf.getFrontFace(1)
report(ff1, "ff1.map")

fl5 = mf.getLowerFace(5)
report(fl5, "fl5.map")
fl3 = mf.getLowerFace(3)
report(fl3, "fl3.map")
