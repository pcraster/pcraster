#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pcraster import *
from pcraster.framework import *

class RunoffModel(DynamicModel):#, MonteCarloModel):
  def __init__(self, cloneMap):
    DynamicModel.__init__(self)
    #MonteCarloModel.__init__(self)
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

    # initialise timeoutput
    self.runoffTss_file = TimeoutputTimeseries("runoff_file", self, "samples.map", noHeader=False)

    loc =  readmap("samples.map")
    self.runoffTss_field = TimeoutputTimeseries("runoff_field", self, loc, noHeader=True)

    loc2 =  readmap("samples2.map")
    self.runoffTss_field_mv = TimeoutputTimeseries("runoff_field_mv", self, loc2)
    self.runoffTss_woLoc = TimeoutputTimeseries("runoff_field_woLoc", self, None, noHeader=False)

  def dynamic(self):
    # calculate and report maps with rainfall at each timestep (mm/6 hours)
    surfaceWater = timeinputscalar("rain.tss", self.rainZones)
    self.report(surfaceWater, "rainfall")

    # compute both runoff and actual infiltration
    runoff = accuthresholdflux(self.ldd, surfaceWater,\
         self.infiltrationCapacity)
    infiltration = accuthresholdstate(self.ldd, surfaceWater,\
         self.infiltrationCapacity)

    # output runoff, converted to m3/s, at each timestep
    logRunOff = runoff / scalar(216000)
    self.report(logRunOff, "logrunof")
    # tss
    self.runoffTss_file.sample(logRunOff)
    self.runoffTss_file.sample(logRunOff)
    self.runoffTss_field.sample(logRunOff)
    self.runoffTss_field.sample(logRunOff)
    self.runoffTss_field_mv.sample(logRunOff)
    self.runoffTss_woLoc.sample(logRunOff)


class RunoffModelMC(DynamicModel, MonteCarloModel):
  def __init__(self, cloneMap):
    DynamicModel.__init__(self)
    MonteCarloModel.__init__(self)
    setclone(cloneMap)

  def premcloop(self):
    pass

  def postmcloop(self):
    pass

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

    # initialise timeoutput
    self.runoffTss_file = TimeoutputTimeseries("runoff_file", self, "samples.map", noHeader=False)

    loc =  readmap("samples.map")
    self.runoffTss_field = TimeoutputTimeseries("runoff_field", self, loc, noHeader=True)

  def dynamic(self):
    # calculate and report maps with rainfall at each timestep (mm/6 hours)
    surfaceWater = timeinputscalar("rain.tss", self.rainZones)
    self.report(surfaceWater, "rainfall")

    # compute both runoff and actual infiltration
    runoff = accuthresholdflux(self.ldd, surfaceWater,\
         self.infiltrationCapacity)
    infiltration = accuthresholdstate(self.ldd, surfaceWater,\
         self.infiltrationCapacity)

    # output runoff, converted to m3/s, at each timestep
    logRunOff = runoff / scalar(216000)
    self.report(logRunOff, "logrunof")
    # tss
    self.runoffTss_file.sample(logRunOff)
    self.runoffTss_file.sample(logRunOff)
    self.runoffTss_field.sample(logRunOff)
    self.runoffTss_field.sample(logRunOff)


class FloatReport(DynamicModel, MonteCarloModel):
  def __init__(self, cloneMap):
    DynamicModel.__init__(self)
    MonteCarloModel.__init__(self)
    setclone(cloneMap)

  def initial(self):
    self.runoffTss_file = TimeoutputTimeseries("runoff_file", self, "samples.map", noHeader=False)
    self.floatVal = 2.2

  def dynamic(self):
    self.runoffTss_file.sample(self.floatVal)
