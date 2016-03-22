from pcraster import *
from pcraster.framework import *

class VegetationGrowthModel(StaticModel, MonteCarloModel):
  def __init__(self):
    StaticModel.__init__(self)
    MonteCarlo.__init__(self)
    setclone("clone.map")

  def premcloop(self):
    pass

  def initial(self):
    # spreading time for peat (years)
    peatYears = 0.1 + mapnormal() * 0.001
    # spreading time for other soil types (years)
    otherYears = 0.5 + mapnormal() * 0.02
    # number of years needed to move the vegetation front 1 m
    years = ifthenelse("peat.map", peatYears, otherYears)
    # time to colonization (yr)
    colTime = spread("distr.map", years)
    # colonized after 50 years?
    col = ifthen(colTime < 50)
    self.report(col, "col")

  def postmcloop(self):
    names = ["col"]
    mcaveragevariance(names, "", "")

myModel = VegetationGrowthModel()
staticModel = StaticFramework(myModel)
mcModel = MonteCarloFramework(staticModel, 100)
mcModel.run()
