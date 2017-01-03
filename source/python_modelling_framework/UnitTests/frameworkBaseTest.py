#!/usr/bin/env python
# -*- coding: utf-8 -*-

import unittest
import os

import pcraster.framework.frameworkBase as frameworkBase
from pcraster.framework.frameworkBase import FrameworkError


class frameworkBaseTestScript(unittest.TestCase):
  def testGenerateName(self):
    name = frameworkBase.generateNameT("piet", 5)
    self.assertEqual(name, "piet0000.005")

    name = frameworkBase.generateNameT(os.path.join("some","dir","somewhere","piet"), 1055)
    self.assertEqual(name, os.path.join("some","dir","somewhere","piet0001.055"))

    name = frameworkBase.generateNameS("piet", 5)
    self.assertEqual(name, os.path.join(str(5), "piet"))

    name = frameworkBase.generateNameST("piet", 5, 1055)
    self.assertEqual(name, os.path.join(str(5), "piet0001.055"))

    try:
      name = frameworkBase.generateNameT("pietje.map" , 5)
    except FrameworkError as e:
      self.assertEqual(str(e), "File extension given in 'pietje.map' not allowed")

    try:
      name = frameworkBase.generateNameT("" , 5)
    except FrameworkError as e:
      self.assertEqual(str(e), "No filename specified")

    try:
      name = frameworkBase.generateNameT("verylongname" , 5)
    except FrameworkError as e:
      self.assertEqual(str(e), "Filename 'verylongname' must be shorter than 8 characters")

    try:
      name = frameworkBase.generateNameT("neg" , -5)
    except FrameworkError as e:
      self.assertEqual(str(e), "Timestep must be larger than 0")


