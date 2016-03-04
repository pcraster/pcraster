#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pcraster import *
from pcraster.framework import *

class SnowModel(DynamicModel, MonteCarloModel, ParticleFilterModel):
  def __init__(self):
    DynamicModel.__init__(self)
    MonteCarloModel.__init__(self)
    ParticleFilterModel.__init__(self)
    setclone("clone.map")

  def premcloop(self):
    dem = self.readmap("dem")
    self.ldd = lddcreate(dem, 1e31, 1e31, 1e31, 1e31)
    elevationMeteoStation = scalar(2058.1)
    self.elevationAboveMeteoStation = dem - elevationMeteoStation
    self.degreeDayFactor = 0.01

  def initial(self):
    self.snow = scalar(0)
    self.temperatureLapseRate = 0.005 + (mapnormal() * 0.001)
    self.report(self.temperatureLapseRate, "lapse")
    self.temperatureCorrection = self.elevationAboveMeteoStation\
         * self.temperatureLapseRate

  def dynamic(self):
    temperatureObserved = self.readDeterministic("tavgo")
    precipitationObserved = self.readDeterministic("pr")
    precipitation = max(0, precipitationObserved * (mapnormal() * 0.2 + 1.0))
    temperature = temperatureObserved - self.temperatureCorrection
    snowFall = ifthenelse(temperature < 0, precipitation, 0)
    self.snow = self.snow + snowFall
    potentialMelt = ifthenelse(temperature > 0, temperature\
         * self.degreeDayFactor, 0)
    actualMelt = min(self.snow, potentialMelt)
    self.snow = max(0, self.snow - actualMelt)
    self.report(self.snow, "s")

  def postmcloop(self):
    names = ["s"]
    mcaveragevariance(names, self.sampleNumbers(), self.timeSteps())
    percentiles = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
    mcpercentiles(names, percentiles, self.sampleNumbers(), self.timeSteps())

  def updateWeight(self):
    modelledData = self.readmap("s")
    modelledAverageMap = areaaverage(modelledData, "zones.map")
    observedAverageMap = self.readDeterministic("obsAv")
    observedStdDevMap = ifthenelse(observedAverageMap > 0, observedAverageMap\
         * 0.4, 0.01)
    sum = maptotal(((observedAverageMap - modelledAverageMap) ** 2) / (-2.0\
         * (observedStdDevMap ** 2)))
    weight = exp(sum)
    weightFloatingPoint, valid = cellvalue(weight, 1, 1)
    return weightFloatingPoint

  def suspend(self):
    self.reportState(self.temperatureLapseRate, "lapse")
    self.reportState(self.snow, "s")

  def resume(self):
    self.temperatureLapseRate = self.readState("lapse")
    self.temperatureCorrection = self.elevationAboveMeteoStation\
         * self.temperatureLapseRate
    self.snow = self.readState("s")

myModel = SnowModel()
dynamicModel = DynamicFramework(myModel, lastTimeStep=180, firstTimestep=1)
mcModel = MonteCarloFramework(dynamicModel, nrSamples=10)
pfModel = SequentialImportanceResamplingFramework(mcModel)
#pfModel = ResidualResamplingFramework(mcModel)
pfModel.setFilterTimesteps([70, 100, 150])
pfModel.run()
