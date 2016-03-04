#!/usr/bin/env python
# -*- coding: utf-8 -*-

# static model

from pcraster import *
from pcraster.framework import *

class RunoffModel(StaticModel):
  def __init__(self, cloneMap):
    StaticModel.__init__(self)
    setclone(cloneMap)

  def initial(self):
    # coverage of meteorological stations for the whole area
    self.rainZones = spreadzone("rainstat.map", scalar(0), scalar(1))

    # create an infiltration capacity map (mm/6 hours), based on the
    # soil map
    self.infiltrationCapacity = lookupscalar("infilcap.tbl", "soil.map")
    self.report(self.infiltrationCapacity, "infilcap")

    # generate the local drain direction map on basis of the elevation map
    self.ldd = lddcreate("dem.map", 1e31, 1e31, 1e31, 1e31)
    self.report(self.ldd, "ldd")

myModel = RunoffModel("mask.map")
stModelFw = StaticFramework(myModel)
stModelFw.run()
