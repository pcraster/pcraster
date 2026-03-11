import pcraster as pcr
import pcraster.framework as pcrfw


class SnowModel(pcrfw.DynamicModel, pcrfw.MonteCarloModel, pcrfw.ParticleFilterModel):
    def __init__(self):
        pcrfw.DynamicModel.__init__(self)
        pcrfw.MonteCarloModel.__init__(self)
        pcrfw.ParticleFilterModel.__init__(self)
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

    def updateWeight(self):
        modelledData = self.readmap("s")
        modelledAverageMap = pcr.areaaverage(modelledData, "zones.map")
        observedAverageMap = self.readDeterministic("obsAv")
        observedStdDevMap = pcr.ifthenelse(observedAverageMap > 0, observedAverageMap * 0.4, 0.01)
        sum = pcr.maptotal(((observedAverageMap - modelledAverageMap) ** 2) / (-2.0 * (observedStdDevMap ** 2)))
        weight = pcr.exp(sum)
        weightFloatingPoint, valid = pcr.cellvalue(weight, 1, 1)
        return weightFloatingPoint

    def suspend(self):
        self.reportState(self.temperatureLapseRate, "lapse")
        self.reportState(self.snow, "s")

    def resume(self):
        self.temperatureLapseRate = self.readState("lapse")
        self.temperatureCorrection = self.elevationAboveMeteoStation * self.temperatureLapseRate
        self.snow = self.readState("s")


myModel = SnowModel()
dynamicModel = pcrfw.DynamicFramework(myModel, lastTimeStep=180, firstTimestep=1)
mcModel = pcrfw.MonteCarloFramework(dynamicModel, nrSamples=10)
pfModel = pcrfw.SequentialImportanceResamplingFramework(mcModel)
# pfModel = pcrfw.ResidualResamplingFramework(mcModel)
pfModel.setFilterTimesteps([70, 100, 150])
pfModel.run()
