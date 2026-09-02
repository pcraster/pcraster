# based on bcf2ss
from pcraster import *

setclone(pathlib.Path("bcf2ss_clone.map"))
bcf2ss = initialise(clone())

bcf2ss.createBottomLayer(pathlib.Path("l1.pcrmap"), pathlib.Path("l1_top.pcrmap"))
bcf2ss.addConfinedLayer(pathlib.Path("l2_top.pcrmap"))
bcf2ss.addLayer(pathlib.Path("l3_top.pcrmap"))

bcf2ss.setBoundary(pathlib.Path("ib_1.pcrmap"), 1)
bcf2ss.setBoundary(pathlib.Path("ib_3.pcrmap"), 3)
bcf2ss.setInitialHead(pathlib.Path("head.pcrmap"), 1)
bcf2ss.setInitialHead(pathlib.Path("head.pcrmap"), 3)

bcf2ss.setConductivity(0, pathlib.Path("hcond.pcrmap"), pathlib.Path("vcond1.pcrmap"), 1)
bcf2ss.setConductivity(0, pathlib.Path("hcond.pcrmap"), pathlib.Path("vcond2.pcrmap"), 2)
bcf2ss.setConductivity(1, pathlib.Path("hcond3.pcrmap"), pathlib.Path("vcond3.pcrmap"), 3)
bcf2ss.setDryHead(777.77)
bcf2ss.setNoFlowHead(999.99)

bcf2ss.setWettingParameter(1, 1, 0)
bcf2ss.setWetting(pathlib.Path("bcf2ss_wet.map"), 3)

bcf2ss.setRecharge(pathlib.Path("rch.pcrmap"), 3)

bcf2ss.setRiver(pathlib.Path("rh.pcrmap"), pathlib.Path("rb.pcrmap"), pathlib.Path("rc.pcrmap"), 1)

bcf2ss.setDISParameter(4, 0, 1, 1, 1, 1)

bcf2ss.setPCG(40, 20, 1, 0.001, 1000.0, 1, 2, 1)

bcf2ss.setRowWidth([5000.0, 5001.01, 5002.02, 5003.03, 5004.04, 5005.05, 5006.06, 5007.07, 5008.08, 5009.09])
bcf2ss.setColumnWidth([2000.0, 2001.01, 2002.02, 2003.03, 2004.04, 2005.05, 2006.06, 2007.07, 2008.08, 2009.09, 2010.1, 2011.11, 2012.12, 2013.13, 2014.14])

# time step 1
# execute in subdirectory 1
bcf2ss.run(pathlib.Path("rowcol"))
