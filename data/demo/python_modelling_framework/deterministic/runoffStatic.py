# static model
import pcraster as pcr
import pcraster.framework as pcrfw


class RunoffModel(pcrfw.StaticModel):
    def __init__(self, cloneMap):
        pcrfw.StaticModel.__init__(self)
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


myModel = RunoffModel("mask.map")
stModelFw = pcrfw.StaticFramework(myModel)
stModelFw.run()
