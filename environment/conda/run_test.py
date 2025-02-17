import pcraster as pcr
import pcraster.framework as pcrfw
import pcraster.multicore as pcrmc
import pcraster.mldd as pcrmldd
import pcraster.collection as pcrcol
import pcraster.moc as pcrmoc

pcr.setclone(5, 4, 3, 2, 1)

raster = pcr.uniform(1)

pcrmc.set_nr_worker_threads(2)

class UserModel(pcrfw.StaticModel):
    def __init__(self):
        pcrfw.StaticModel.__init__(self)

    def initial(self):
        print(f"PCRaster version: {pcr.__version__}")

myModel = UserModel()
staticModel = pcrfw.StaticFramework(myModel)
staticModel.run()
