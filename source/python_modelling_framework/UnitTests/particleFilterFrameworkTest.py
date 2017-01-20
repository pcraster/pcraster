#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import testcase
import unittest
import pcraster.framework.staticFramework as sf
import pcraster.framework.dynamicFramework as df
import pcraster.framework.frameworkBase as fb
import pcraster.framework.mcFramework as mc
import pcraster.framework.particleFilterFramework as pf
import pfTestModels


## \brief unit tests dynamic framework
class ParticleFilterFrameworkTestScript(testcase.TestCase):
  def test_001(self):
    """test requirements of the user model class"""
    staticModel = pfTestModels.StaticWithoutAll()
    staticModelFrw = sf.StaticFramework(staticModel)
    try:
      pfModel = pf.SequentialImportanceResamplingFramework(staticModelFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Model must be instance of MonteCarloFramework")


    # updateWeight
    staticModel = pfTestModels.StaticWithoutAll()
    staticModelFrw = sf.StaticFramework(staticModel)
    staticMcFrw = mc.MonteCarloFramework(staticModelFrw, 5)
    try:
      pfModel = pf.SequentialImportanceResamplingFramework(staticMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'updateWeight' method")
    try:
      pfModel = pf.ResidualResamplingFramework(staticMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'updateWeight' method")

    dynamicModel = pfTestModels.DynamicWithoutAll()
    dynamicModelFrw = sf.StaticFramework(dynamicModel)
    dynamicMcFrw = mc.MonteCarloFramework(dynamicModelFrw, 5)
    try:
      pfModel = pf.SequentialImportanceResamplingFramework(dynamicMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'updateWeight' method")
    try:
      pfModel = pf.ResidualResamplingFramework(dynamicMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'updateWeight' method")


    # suspend
    staticModel = pfTestModels.StaticWithoutSuspend()
    staticModelFrw = sf.StaticFramework(staticModel)
    staticMcFrw = mc.MonteCarloFramework(staticModelFrw, 5)
    try:
      pfModel = pf.SequentialImportanceResamplingFramework(staticMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'suspend' method")
    try:
      pfModel = pf.ResidualResamplingFramework(staticMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'suspend' method")

    dynamicModel = pfTestModels.DynamicWithoutSuspend()
    dynamicModelFrw = sf.StaticFramework(dynamicModel)
    dynamicMcFrw = mc.MonteCarloFramework(dynamicModelFrw, 5)
    try:
      pfModel = pf.SequentialImportanceResamplingFramework(dynamicMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'suspend' method")
    try:
      pfModel = pf.ResidualResamplingFramework(dynamicMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'suspend' method")


    # resume
    staticModel = pfTestModels.StaticWithoutResume()
    staticModelFrw = sf.StaticFramework(staticModel)
    staticMcFrw = mc.MonteCarloFramework(staticModelFrw, 5)
    try:
      pfModel = pf.SequentialImportanceResamplingFramework(staticMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'resume' method")
    try:
      pfModel = pf.ResidualResamplingFramework(staticMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'resume' method")

    dynamicModel = pfTestModels.DynamicWithoutResume()
    dynamicModelFrw = sf.StaticFramework(dynamicModel)
    dynamicMcFrw = mc.MonteCarloFramework(dynamicModelFrw, 5)
    try:
      pfModel = pf.SequentialImportanceResamplingFramework(dynamicMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'resume' method")
    try:
      pfModel = pf.ResidualResamplingFramework(dynamicMcFrw)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run particle filter framework: Implement 'resume' method")


  @unittest.expectedFailure
  def test_002(self):
    """ value of Python variables in the resume section, SIR """
    dynamicModel = pfTestModels.DynamicModel()
    dynamicModelFrw = df.DynamicFramework(dynamicModel, lastTimeStep=10, firstTimestep=1)
    dynamicModelFrw.setQuiet()
    dynamicMcFrw = mc.MonteCarloFramework(dynamicModelFrw, nrSamples=5)
    dynamicMcFrw.setQuiet()
    runPassed = False
    try:
      pfModel = pf.SequentialImportanceResamplingFramework(dynamicMcFrw)
      pfModel.setFilterTimesteps([3, 5, 7])
      pfModel.setQuiet(True)
      pfModel.run()
      runPassed = True
    except AssertionError:
      pass

    self.assertTrue(runPassed)


  @unittest.expectedFailure
  def test_003(self):
    """ value of Python variables in the resume section, RR """
    dynamicModel = pfTestModels.DynamicModel()
    dynamicModelFrw = df.DynamicFramework(dynamicModel, lastTimeStep=10, firstTimestep=1)
    dynamicModelFrw.setQuiet()
    dynamicMcFrw = mc.MonteCarloFramework(dynamicModelFrw, nrSamples=5)
    dynamicMcFrw.setQuiet()
    runPassed = False
    try:
      pfModel = pf.ResidualResamplingFramework(dynamicMcFrw)
      pfModel.setFilterTimesteps([3, 5, 7])
      pfModel.setQuiet(True)
      pfModel.run()
      runPassed = True
    except AssertionError:
      pass

    self.assertTrue(runPassed)
