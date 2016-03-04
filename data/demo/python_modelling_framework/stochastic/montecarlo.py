#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pcraster import *
from pcraster.framework import *

class SnowModel(DynamicModel, MonteCarloModel):
  def __init__(self):
    DynamicModel.__init__(self)
    MonteCarloModel.__init__(self)
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
    rain = ifthenelse(temperature >= 0, precipitation, 0)
    discharge = accuflux(self.ldd, actualMelt + rain)
    self.report(self.snow, "s")
    self.report(discharge, "q")

  def postmcloop(self):
    names = ["s", "q"]
    mcaveragevariance(names, self.sampleNumbers(), self.timeSteps())
    percentiles = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
    mcpercentiles(names, percentiles, self.sampleNumbers(), self.timeSteps())

myModel = SnowModel()
dynamicModel = DynamicFramework(myModel, lastTimeStep=180, firstTimestep=1)
mcModel = MonteCarloFramework(dynamicModel, nrSamples=10)
mcModel.run()
