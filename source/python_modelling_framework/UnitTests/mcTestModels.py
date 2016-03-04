#!/usr/bin/env python
# -*- coding: utf-8 -*-
import random
import numpy.random
import pcraster
import pcraster.framework.dynamicPCRasterBase as dynamicPCRasterBase
import pcraster.framework.mcPCRasterBase as mcPCRasterBase
import pcraster.framework.staticPCRasterBase as staticPCRasterBase


#
class T0(mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    mcPCRasterBase.MonteCarloModel.__init__(self)


#
class T1(mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    mcPCRasterBase.MonteCarloModel.__init__(self)


#
class staticModel(staticPCRasterBase.StaticModel, mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    staticPCRasterBase.StaticModel.__init__(self)
    mcPCRasterBase.MonteCarloModel.__init__(self)
    pcraster.setclone("clone.map")
    self.newmap = pcraster.readmap("clone.map")

  def initial(self):
    name = "mcsi%d" % (self.currentSampleNumber())
    self.report(self.newmap, name)

  def premcloop(self):
    for sample in self.sampleNumbers():
      name = "premc%d" % (sample)
      self.report(self.newmap, name)

  def postmcloop(self):
    for sample in self.sampleNumbers():
      name = "postmc%d" % (sample)
      self.report(self.newmap, name)


#
class dynamicModel(dynamicPCRasterBase.DynamicModel, mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    mcPCRasterBase.MonteCarloModel.__init__(self)
    dynamicPCRasterBase.DynamicModel.__init__(self)
    pcraster.setclone("clone.map")
    self.newmap = pcraster.readmap("clone.map")

  def initial(self):
    name = "mcdi%d" % (self.currentSampleNumber())
    self.report(self.newmap, name)

  def premcloop(self):
    for sample in self.sampleNumbers():
      for timestep in self.timeSteps():
        name = "premc_%d_%d" % (sample, timestep)
        self.report("clone.map", name)

  def postmcloop(self):
    for sample in self.sampleNumbers():
      for timestep in self.timeSteps():
        name = "postmc_%d_%d" % (sample, timestep)
        self.report("clone.map", name)

  def dynamic(self):
    name = "mcdd%d" % (self.currentSampleNumber())
    self.report("clone.map", name)


class randomModel(dynamicPCRasterBase.DynamicModel, mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    dynamicPCRasterBase.DynamicModel.__init__(self)
    mcPCRasterBase.MonteCarloModel.__init__(self)
    pcraster.setclone("clone.map")
  def dynamic(self):
    pass
  def postmcloop(self):
    pass
  def premcloop(self):
    pass
  def initial(self):
    pythonVal = pcraster.scalar(random.random())
    self.report(pythonVal, "pyVal")
    pcrVal = pcraster.mapnormal()
    self.report(pcrVal, "pcrVal")
    numpyVal = pcraster.scalar(numpy.random.random())
    self.report(numpyVal, "npVal")
