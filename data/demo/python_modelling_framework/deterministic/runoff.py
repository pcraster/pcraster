# model for simulation of runoff
# 24 timesteps of 6 hours => modelling time one week
import pcraster as pcr
import pcraster.framework as pcrfw


class RunoffModel(pcrfw.DynamicModel):
    def __init__(self, cloneMap):
        pcrfw.DynamicModel.__init__(self)
        pcr.setclone(cloneMap)

    def initial(self):
        # coverage of meteorological stations for the whole area
        self.rainZones = pcr.spreadzone("rainstat.map", pcr.scalar(0), pcr.scalar(1))

        # create an infiltration capacity map (mm/6 hours), based on the
        # soil map
        self.infiltrationCapacity = pcr.lookupscalar("infilcap.tbl", "soil.map")
        self.report(self.infiltrationCapacity, "infilcap")

        # generate the local drain direction map on basis of the elevation map
        self.ldd = pcr.lddcreate("dem.map", 1e31, 1e31, 1e31, 1e31)
        self.report(self.ldd, "ldd")

        # initialise timeoutput
        self.runoffTss = pcrfw.TimeoutputTimeseries("runoff", self, "samples.map", noHeader=False)

    def dynamic(self):
        # calculate and report maps with rainfall at each timestep (mm/6 hours)
        surfaceWater = pcr.timeinputscalar("rain.tss", self.rainZones)
        self.report(surfaceWater, "rainfall")

        # compute both runoff and actual infiltration
        runoff = pcr.accuthresholdflux(self.ldd, surfaceWater, self.infiltrationCapacity)
        infiltration = pcr.accuthresholdstate(self.ldd, surfaceWater, self.infiltrationCapacity)

        # output runoff, converted to m3/s, at each timestep
        logRunOff = runoff / pcr.scalar(216000)
        self.report(logRunOff, "logrunof")
        # sampling timeseries for given locations
        self.runoffTss.sample(logRunOff)


myModel = RunoffModel("mask.map")
dynModelFw = pcrfw.DynamicFramework(myModel, lastTimeStep=28, firstTimestep=1)
dynModelFw.run()
