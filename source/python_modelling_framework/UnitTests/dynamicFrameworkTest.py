#!/usr/bin/env python
# -*- coding: utf-8 -*-
import testcase
import sys
import pcraster
import pcraster.framework.dynamicPCRasterBase
import pcraster.framework.dynamicFramework as df
import pcraster.framework.frameworkBase
import dynamicTestModels


## \brief unit tests dynamic framework
class dynamicFrameworkTestScript(testcase.TestCase):
  def test_1(self):
    """test existence of added methods and attributes"""
    myModel = dynamicTestModels.T1()
    self.assertEqual(sys.getrefcount(myModel), 2)
    dynModelFw = df.DynamicFramework(myModel, 5)
    self.assertEqual(sys.getrefcount(myModel), 3)
    self.assertEqual(sys.getrefcount(dynModelFw), 2)
    self.assert_(hasattr(myModel, "timeSteps"))
    self.assert_(hasattr(myModel, "nrTimeSteps"))
    self.assert_(hasattr(myModel, "currentTimeStep"))
    self.assert_(hasattr(myModel, "setQuiet"))
    self.assert_(hasattr(myModel, "setDebug"))
    self.assert_(hasattr(myModel, "_setFirstTimeStep"))
    self.assert_(hasattr(myModel, "_inTimeStep"))
    self.assert_(hasattr(myModel, "_setCurrentTimeStep"))
    self.assert_(hasattr(myModel, "_setNrTimeSteps"))
    self.assert_(hasattr(myModel, "report"))
    self.assert_(hasattr(myModel, "_d_nrTimeSteps"))
    self.assert_(hasattr(myModel, "currentStep"))
    self.assert_(hasattr(myModel, "_d_firstTimeStep"))
    self.assert_(hasattr(myModel, "inTimeStep"))

  def test_2(self):
    """test existence of dynamic method"""
    myModel = dynamicTestModels.T1()
    try:
      dynModelFw = df.DynamicFramework(myModel, 10)
    except pcraster.framework.frameworkBase.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run dynamic framework: Implement \
dynamic method")

  def test_3(self):
    """test executing initial and dynamic methods"""
    myModel = dynamicTestModels.T2()
    dynModelFw = df.DynamicFramework(myModel, 10)
    dynModelFw.setQuiet(True)
    dynModelFw.run()
    self.assertEqual(myModel.initialValue, 1)
    self.assertEqual(myModel.value, 11)


  def test_4(self):
    """test modification of timesteps"""
    myModel = dynamicTestModels.T2()
    dynModelFw = df.DynamicFramework(myModel, 10)
    self.assertEqual(myModel.timeSteps(), range(1,11))
    self.assertEqual(myModel.nrTimeSteps(), 10)
    myModel._setNrTimeSteps(15)
    self.assertEqual(myModel.nrTimeSteps(), 15)
    self.assertEqual(myModel.timeSteps(), range(1,16))

    self.assertEqual(myModel.currentTimeStep(), 0)
    self.assertEqual(myModel.firstTimeStep(), 1)

    myModel._setFirstTimeStep(3)
    myModel._setCurrentTimeStep(7)
    self.assertEqual(myModel.firstTimeStep(), 3)
    self.assertEqual(myModel.currentTimeStep(), 7)
    self.assertEqual(myModel.nrTimeSteps(), 15)
    self.assertEqual(myModel.timeSteps(), range(3,16))

  def test_5(self):
    """ test setting different start time"""
    # without modifications
    myModel = dynamicTestModels.TestStartTime()
    dynModelFw = df.DynamicFramework(myModel, 10)
    dynModelFw.setQuiet(True)
    dynModelFw.run()
    self.assertEqual(myModel.firstTimeStep(), 1)
    self.assertEqual(myModel.value, 1520)
    self.assertEqual(myModel.initValue, 2)

    # with new starting timestep
    myModel2 = dynamicTestModels.TestStartTime()
    dynModelFw = df.DynamicFramework(myModel2, 10, 5)
    dynModelFw.setQuiet(True)
    dynModelFw.run()
    self.assertEqual(myModel2.firstTimeStep(), 5)
    self.assertEqual(myModel2.value, 726)
    self.assertEqual(myModel2.initValue, 2)

  def test_6(self):
    """test executing one timestep"""
    myModel = dynamicTestModels.TestStartTime()
    dynModelFw = df.DynamicFramework(myModel, 1)
    dynModelFw.run()


  def test_7(self):
    """test report/readmap"""
    myModel = dynamicTestModels.TestReadmapReport()
    dynModelFw = df.DynamicFramework(myModel, 5)
    dynModelFw.setQuiet(True)
    dynModelFw.run()

    try:
      result = pcraster.readmap("static.map")
      self.failUnless(self.mapEquals(result, "plus.Result.map"), "test_04: %s" % ("Result and validated result are not the same"))
    except Exception as exception:
      self.failUnless(False, "test1: %s" % (str(exception)))

    for timestep in myModel.timeSteps():
      try:
        name = pcraster.framework.frameworkBase.generateNameT("dyna", timestep)
        result = pcraster.readmap(name)
        self.failUnless(self.mapEquals(result, "plus.Result.map"), "test04: %s" % ("Result and validated result are not the same"))
      except Exception as exception:
        self.failUnless(False, "test1: %s" % (str(exception)))

  def test_8(self):
    """ test lookupscalar in dynamic model [bugzilla 269] """
    class DummyModel(pcraster.framework.dynamicPCRasterBase.DynamicModel):
      def __init__(self, cloneMap):
        pcraster.framework.dynamicPCRasterBase.DynamicModel.__init__(self)
        pcraster.setclone(cloneMap)

      def initial(self):
        pass

      def dynamic(self):
        # rewrite input each timestep
        filename = "in.tbl"
        f = open(filename, "w")
        f.write("1 %f\n" % (2.5 * self.currentTimeStep()))
        f.write("2 %f\n" % (3.5 * self.currentTimeStep()))
        f.write("3 %f\n" % (5.5 * self.currentTimeStep()))
        f.close()
        tmp = pcraster.lookupscalar(filename, "soil.map")
        self.report(tmp, "tmp")

    myModel = DummyModel("mask.map")
    dynModelFw = df.DynamicFramework(myModel, lastTimeStep=5, firstTimestep=1)
    dynModelFw.run()

    for i in range(1,6):
      filename = pcraster.framework.frameworkBase.generateNameT("tmp", i)
      tmp = pcraster.readmap(filename)
      #self.assertEqual(pcraster.cellvalue(tmp, 139)[0], 2.5 * i)
      #self.assertEqual(pcraster.cellvalue(tmp, 606)[0], 3.5 * i)
      #self.assertEqual(pcraster.cellvalue(tmp, 947)[0], 5.5 * i)
