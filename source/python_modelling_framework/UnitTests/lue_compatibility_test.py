import pcraster as pcr
import pcraster.framework.dynamicPCRasterBase as dynamicPCRasterBase
import pcraster.framework.dynamicFramework as df

import testcase


class TestTimeinputscalar(dynamicPCRasterBase.DynamicModel):
    def __init__(self):
        dynamicPCRasterBase.DynamicModel.__init__(self)
        pcr.setclone("clone.map")

    def initial(self):
        self.initValue = 2
        self.value = 0

    def dynamic(self):
        self.raster = pcr.timeinputscalar("timeseries.tss", "clone.map", self.currentTimeStep())


class lue_compatibility_tests(testcase.TestCase):

    def test_1(self):
        """ simple test third argument required by LUE """
        myModel = TestTimeinputscalar()
        dynModelFw = df.DynamicFramework(myModel, 10)
        dynModelFw.setQuiet(True)
        dynModelFw.run()

    def test_2(self):

        @pcr.runtime_scope
        def do_nothing():
            pass
