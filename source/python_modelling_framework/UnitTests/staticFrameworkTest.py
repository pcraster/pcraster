#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os
import sys
import testcase
import pcraster
import pcraster.framework.staticFramework as sf
from pcraster.framework.frameworkBase import FrameworkError
import staticTestModels


## \brief unit tests static framework
class staticFrameworkTestScript(testcase.TestCase):
  def test_1(self):
    """test existence of added methods"""
    myModel = staticTestModels.T2()
    staticModel = sf.StaticFramework(myModel)
    self.assert_(hasattr(myModel, "setDebug"))
    self.assert_(hasattr(myModel, "report"))
  #  self.assert_(hasattr(myModel, "readmap"), "Todo")


  def test_2(self):
    """test existence of initial method"""
    myModel = staticTestModels.T1()
    try:
      staticModel = sf.StaticFramework(myModel)
    except FrameworkError as e:
      self.assertEqual(str(e),"Cannot run static framework: Implement either an initial or a run method in the user class")


  def test_3(self):
    """test execution of initial method"""
    myModel = staticTestModels.T2()
    staticModelFw = sf.StaticFramework(myModel)
    self.assertEqual(myModel.value, 0)
    staticModelFw.run()
    self.assertEqual(myModel.value, 1)


  def test_4(self):
    """test self.readmap/self.report functionality"""
    myModel = staticTestModels.ReadmapReport()
    staticModelFw = sf.StaticFramework(myModel)
    staticModelFw.run()
    try:
      result = pcraster.readmap("static.map")
      self.failUnless(self.mapEquals(result, "plus.Result.map"), "test_4: %s" % ("Result and validated result are not the same"))
    except Exception as exception:
      self.failUnless(False, "test4: %s" % (str(exception)))


  def test_5(self):
    #class StaticPCRasterModel(frameworkMetaClasses.StaticMetaClass):
      #def __init__(self):
    #  pass
    myModel = staticTestModels.Static1()
    staticModelFw = sf.StaticFramework(myModel)
    staticModelFw.run()
