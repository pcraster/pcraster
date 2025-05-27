import pcraster as pcr
import pcraster.framework.dynamicPCRasterBase as dynamicPCRasterBase
import pcraster.framework.dynamicFramework as df

import testcase


class TestTimeinputscalar(dynamicPCRasterBase.DynamicModel):
    def __init__(self):
        dynamicPCRasterBase.DynamicModel.__init__(self)
        pcr.setclone(5, 4, 3, 2, 1)

    def initial(self):
        pass

    def dynamic(self):
        self.raster = pcr.timeinputscalar("timeseries.tss", 1, self.currentTimeStep())


class lue_compatibility_tests(testcase.TestCase):

    def test_1(self):
        """ Test third argument timeinputscalar required by LUE """
        myModel = TestTimeinputscalar()
        dynModelFw = df.DynamicFramework(myModel, 10)
        dynModelFw.setQuiet(True)
        dynModelFw.run()

    def test_2(self):
        """ Test runtime_scope decorator """
        @pcr.runtime_scope
        def do_nothing():
            myModel = TestTimeinputscalar()
            dynModelFw = df.DynamicFramework(myModel, 10)
            dynModelFw.setQuiet(True)
            dynModelFw.run()

        do_nothing()

    def test_3(self):
        """ Test pcraster_provider """
        pr_pcr, pr_pcrfw = pcr.pcraster_provider()

        pr_pcr.setclone(5, 4, 3, 2, 1)
        myModel = TestTimeinputscalar()
        dynModelFw = pr_pcrfw.DynamicFramework(myModel, 10)
        dynModelFw.run()
