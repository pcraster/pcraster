import pcraster as pcr
import pcraster.framework as pcrfw


class SnowModel(pcrfw.DynamicModel, pcrfw.MonteCarloModel):
    def __init__(self):
        pcrfw.DynamicModel.__init__(self)
        pcrfw.MonteCarloModel.__init__(self)
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
        rain = pcr.ifthenelse(temperature >= 0, precipitation, 0)
        discharge = pcr.accuflux(self.ldd, actualMelt + rain)
        self.report(self.snow, "s")
        self.report(discharge, "q")

    def postmcloop(self):
        names = ["s", "q"]
        pcrfw.mcaveragevariance(names, self.sampleNumbers(), self.timeSteps())
        percentiles = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
        pcrfw.mcpercentiles(names, percentiles, self.sampleNumbers(), self.timeSteps())


myModel = SnowModel()
dynamicModel = pcrfw.DynamicFramework(myModel, lastTimeStep=180, firstTimestep=1)
mcModel = pcrfw.MonteCarloFramework(dynamicModel, nrSamples=10)
mcModel.run()
