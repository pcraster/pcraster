import pcraster as pcr
import pcraster.framework as pcrfw


class VegetationGrowthModel(pcrfw.StaticModel, pcrfw.MonteCarloModel):
    def __init__(self):
        pcrfw.StaticModel.__init__(self)
        pcrfw.MonteCarloModel.__init__(self)
        pcr.setclone("clone.map")

    def premcloop(self):
        pass

    def initial(self):
        # spreading time for peat (years)
        peatYears = 0.1 + pcr.mapnormal() * 0.001
        # spreading time for other soil types (years)
        otherYears = 0.5 + pcr.mapnormal() * 0.02
        # number of years needed to move the vegetation front 1 m
        years = pcr.ifthenelse("peat.map", peatYears, otherYears)
        # time to colonization (yr)
        colTime = pcr.spread("distr.map", years)
        # colonized after 50 years?
        col = pcr.ifthen(colTime < 50)
        self.report(col, "col")

    def postmcloop(self):
        names = ["col"]
        pcrfw.mcaveragevariance(names, "", "")


myModel = VegetationGrowthModel()
staticModel = pcrfw.StaticFramework(myModel)
mcModel = pcrfw.MonteCarloFramework(staticModel, 100)
mcModel.run()
