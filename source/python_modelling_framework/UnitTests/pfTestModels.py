#!/usr/bin/env python
# -*- coding: utf-8 -*-
import random
import pcraster
import pcraster.framework.dynamicPCRasterBase as dynamicPCRasterBase
import pcraster.framework.mcPCRasterBase as mcPCRasterBase
import pcraster.framework.pfPCRasterBase as pfPCRasterBase
import pcraster.framework.staticPCRasterBase as staticPCRasterBase


class StaticWithoutAll(staticPCRasterBase.StaticModel, mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    staticPCRasterBase.StaticModel.__init__(self)
    mcPCRasterBase.MonteCarloModel.__init__(self)
  def initial(self):
    pass

class DynamicWithoutAll(dynamicPCRasterBase.DynamicModel, mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    dynamicPCRasterBase.DynamicModel.__init__(self)
    mcPCRasterBase.MonteCarloModel.__init__(self)
  def initial(self):
    pass


class StaticWithoutSuspend(staticPCRasterBase.StaticModel, mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    staticPCRasterBase.StaticModel.__init__(self)
    mcPCRasterBase.MonteCarloModel.__init__(self)
  def initial(self):
    pass
  def updateWeight(self):
    pass

class DynamicWithoutSuspend(dynamicPCRasterBase.DynamicModel, mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    dynamicPCRasterBase.DynamicModel.__init__(self)
    mcPCRasterBase.MonteCarloModel.__init__(self)
  def initial(self):
    pass
  def updateWeight(self):
    pass


class StaticWithoutResume(mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    pass
  def initial(self):
    pass
  def updateWeight(self):
    pass
  def suspend(self):
    pass

class DynamicWithoutResume(mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    pass
  def initial(self):
    pass
  def updateWeight(self):
    pass
  def suspend(self):
    pass

#
class T0(mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    pass


#
class T1(mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    pass


#
class staticModel(mcPCRasterBase.MonteCarloModel):
  def __init__(self):
    mcPCRasterBase.MonteCarloModel.__init__(self)
    staticPCRasterBase.StaticModel.__init__(self)
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
class DynamicModel(dynamicPCRasterBase.DynamicModel, mcPCRasterBase.MonteCarloModel, pfPCRasterBase.ParticleFilterModel):
  def __init__(self):
    dynamicPCRasterBase.DynamicModel.__init__(self)
    mcPCRasterBase.MonteCarloModel.__init__(self)
    pfPCRasterBase.ParticleFilterModel.__init__(self)
    pcraster.setclone("clone.map")
    self.newmap = pcraster.readmap("clone.map")

  def initial(self):
    name = "mcdi%d" % (self.currentSampleNumber())
    self.report(self.newmap, name)
    self.stateVar = self.currentSampleNumber()

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

  def updateWeight(self):
    return random.random()

  def suspend(self):
    assert self.stateVar == self.currentSampleNumber()

  def resume(self):
    assert self.stateVar == self.currentSampleNumber()

