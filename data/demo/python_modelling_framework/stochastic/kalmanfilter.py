#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pcraster import *
from pcraster.framework import *
from numpy import *

class SnowModel(DynamicModel, MonteCarloModel, EnKfModel):
  def __init__(self):
    DynamicModel.__init__(self)
    MonteCarloModel.__init__(self)
    EnKfModel.__init__(self)
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
    percentiles = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
    mcpercentiles(names, percentiles, self.sampleNumbers(), self.timeSteps())

  def setState(self):
    modelledData = self.readmap("s")
    modelledAverageMap = areaaverage(modelledData, "zones.map")
    self.report(modelledAverageMap, "modAv")
    values = numpy.zeros(5)
    values[0] = cellvalue(modelledAverageMap, 5, 5)[0]
    values[1] = cellvalue(modelledAverageMap, 8, 14)[0]
    values[2] = cellvalue(modelledAverageMap, 23, 24)[0]
    values[3] = cellvalue(modelledAverageMap, 28, 12)[0]
    values[4] = cellvalue(modelledAverageMap, 34, 28)[0]
    return values

  def setObservations(self):
    timestep = self.currentTimeStep()
    observedData = readmap(generateNameT("obsAv", timestep))
    values = numpy.zeros(5)
    values[0] = cellvalue(observedData, 1, 1)[0]
    values[1] = cellvalue(observedData, 3, 1)[0]
    values[2] = cellvalue(observedData, 11, 1)[0]
    values[3] = cellvalue(observedData, 18, 1)[0]
    values[4] = cellvalue(observedData, 40, 4)[0]

    # creating the observation matrix (nrObservations x nrSamples)
    # here without added noise
    observations = numpy.array([values,]*self.nrSamples()).transpose()

    # creating the covariance matrix (nrObservations x nrObservations)
    # here just random values
    covariance = numpy.random.random((5, 5))

    self.setObservedMatrices(observations, covariance)

  def resume(self):
    vec = self.getStateVector(self.currentSampleNumber())
    modelledAverageMap = self.readmap("modAv")
    modvalues = numpy.zeros(5)
    modvalues[0] = cellvalue(modelledAverageMap, 1, 1)[0]
    modvalues[1] = cellvalue(modelledAverageMap, 3, 1)[0]
    modvalues[2] = cellvalue(modelledAverageMap, 11, 1)[0]
    modvalues[3] = cellvalue(modelledAverageMap, 18, 1)[0]
    modvalues[4] = cellvalue(modelledAverageMap, 40, 4)[0]
    oldSnowMap = self.readmap("s")
    self.zones = readmap("zones.map")
    newSnowCells = scalar(0)
    for i in range(1, 6):
      snowPerZone = ifthenelse(self.zones == nominal(i), oldSnowMap, scalar(0))
      snowCellsPerZone = ifthenelse(snowPerZone > scalar(0), boolean(1),\
         boolean(0))
      corVal = vec[i - 1] - modvalues[i - 1]
      newSnowCells = ifthenelse(snowCellsPerZone == 1, max(0,snowPerZone\
         + scalar(corVal)), newSnowCells)
    self.snow = newSnowCells


myModel = SnowModel()
dynamicModel = DynamicFramework(myModel, lastTimeStep=180, firstTimestep=1)
mcModel = MonteCarloFramework(dynamicModel, nrSamples=10)
ekfModel = EnsKalmanFilterFramework(mcModel)
ekfModel.setFilterTimesteps([70, 100, 150])
ekfModel.run()
