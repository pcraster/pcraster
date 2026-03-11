import pcraster as pcr
import pcraster.framework as pcrfw
import numpy as np


class SnowModel(pcrfw.DynamicModel, pcrfw.MonteCarloModel, pcrfw.EnKfModel):
    def __init__(self):
        pcrfw.DynamicModel.__init__(self)
        pcrfw.MonteCarloModel.__init__(self)
        pcrfw.EnKfModel.__init__(self)
        pcr.setclone("clone.map")

    def premcloop(self):
        dem = self.readmap("dem")
        self.ldd = pcr.lddcreate(dem, 1e31, 1e31, 1e31, 1e31)
        elevationMeteoStation = pcr.scalar(2058.1)
        self.elevationAboveMeteoStation = dem - elevationMeteoStation
        self.degreeDayFactor = 0.01

    def initial(self):
        self.snow = pcr.scalar(0)
        self.temperatureLapseRate = 0.005 + (pcr.mapnormal() * 0.001)
        self.report(self.temperatureLapseRate, "lapse")
        self.temperatureCorrection = self.elevationAboveMeteoStation * self.temperatureLapseRate

    def dynamic(self):
        temperatureObserved = self.readDeterministic("tavgo")
        precipitationObserved = self.readDeterministic("pr")
        precipitation = pcr.max(0, precipitationObserved * (pcr.mapnormal() * 0.2 + 1.0))
        temperature = temperatureObserved - self.temperatureCorrection
        snowFall = pcr.ifthenelse(temperature < 0, precipitation, 0)
        self.snow = self.snow + snowFall
        potentialMelt = pcr.ifthenelse(temperature > 0, temperature * self.degreeDayFactor, 0)
        actualMelt = pcr.min(self.snow, potentialMelt)
        self.snow = pcr.max(0, self.snow - actualMelt)
        self.report(self.snow, "s")

    def postmcloop(self):
        names = ["s"]
        pcrfw.mcaveragevariance(names, self.sampleNumbers(), self.timeSteps())
        percentiles = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
        pcrfw.mcpercentiles(names, percentiles, self.sampleNumbers(), self.timeSteps())

    def setState(self):
        modelledData = self.readmap("s")
        modelledAverageMap = pcr.areaaverage(modelledData, "zones.map")
        self.report(modelledAverageMap, "modAv")
        values = np.zeros(5)
        values[0] = pcr.cellvalue(modelledAverageMap, 5, 5)[0]
        values[1] = pcr.cellvalue(modelledAverageMap, 8, 14)[0]
        values[2] = pcr.cellvalue(modelledAverageMap, 23, 24)[0]
        values[3] = pcr.cellvalue(modelledAverageMap, 28, 12)[0]
        values[4] = pcr.cellvalue(modelledAverageMap, 34, 28)[0]
        return values

    def setObservations(self):
        timestep = self.currentTimeStep()
        observedData = pcr.readmap(pcrfw.generateNameT("obsAv", timestep))
        values = np.zeros(5)
        values[0] = pcr.cellvalue(observedData, 1, 1)[0]
        values[1] = pcr.cellvalue(observedData, 3, 1)[0]
        values[2] = pcr.cellvalue(observedData, 11, 1)[0]
        values[3] = pcr.cellvalue(observedData, 18, 1)[0]
        values[4] = pcr.cellvalue(observedData, 40, 4)[0]

        # creating the observation matrix (nrObservations x nrSamples)
        # here without added noise
        observations = np.array([values,] * self.nrSamples()).transpose()

        # creating the covariance matrix (nrObservations x nrObservations)
        # here just random values
        covariance = np.random.random((5, 5))

        self.setObservedMatrices(observations, covariance)

    def resume(self):
        vec = self.getStateVector(self.currentSampleNumber())
        modelledAverageMap = self.readmap("modAv")
        modvalues = np.zeros(5)
        modvalues[0] = pcr.cellvalue(modelledAverageMap, 1, 1)[0]
        modvalues[1] = pcr.cellvalue(modelledAverageMap, 3, 1)[0]
        modvalues[2] = pcr.cellvalue(modelledAverageMap, 11, 1)[0]
        modvalues[3] = pcr.cellvalue(modelledAverageMap, 18, 1)[0]
        modvalues[4] = pcr.cellvalue(modelledAverageMap, 40, 4)[0]
        oldSnowMap = self.readmap("s")
        self.zones = pcr.readmap("zones.map")
        newSnowCells = pcr.scalar(0)
        for i in range(1, 6):
            snowPerZone = pcr.ifthenelse(self.zones == pcr.nominal(i), oldSnowMap, pcr.scalar(0))
            snowCellsPerZone = pcr.ifthenelse(snowPerZone > pcr.scalar(0), pcr.boolean(1), pcr.boolean(0))
            corVal = vec[i - 1] - modvalues[i - 1]
            newSnowCells = pcr.ifthenelse(snowCellsPerZone == 1, pcr.max(0, snowPerZone + pcr.scalar(corVal)), newSnowCells)
        self.snow = newSnowCells


myModel = SnowModel()
dynamicModel = pcrfw.DynamicFramework(myModel, lastTimeStep=180, firstTimestep=1)
mcModel = pcrfw.MonteCarloFramework(dynamicModel, nrSamples=10)
ekfModel = pcrfw.EnsKalmanFilterFramework(mcModel)
ekfModel.setFilterTimesteps([70, 100, 150])
ekfModel.run()
