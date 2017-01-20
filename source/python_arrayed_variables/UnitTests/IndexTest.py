#!/usr/bin/env python
# -*- coding: utf-8 -*-
import testcase
import pcraster
import Index as Index



class IndexUnitTests(testcase.TestCase):

  def test1(self):
    """ test multiple indices """
    pcraster.setclone("clone.map")
    exceptionThrown = False
    try:
      PlantSpecies = Index.Index([ "Species1", "Species1", "Species3" ])
    except Exception as e:
      self.assertEqual(str(e), "Error in initialisation of class Index: array indices must be unique, Species1 already used")
      exceptionThrown = True
    self.assert_(exceptionThrown)


  def test2(self):
    """ test change index """
    pcraster.setclone("clone.map")
    exceptionThrown = False
    PlantSpecies = Index.Index([ "Species1", "Species2", "Species3" ])
    try:
      PlantSpecies.Species1 = 5
    except Exception as e:
      self.assertEqual(str(e), "Modification of an Index attribute not permitted")
      exceptionThrown = True
    self.assert_(exceptionThrown)


  def test3(self):
    """ test add index """
    pcraster.setclone("clone.map")
    exceptionThrown = False
    PlantSpecies = Index.Index([ "Species1", "Species2", "Species3" ])
    try:
      PlantSpecies.__setattr__("Species4", 4)
    except Exception as e:
      self.assertEqual(str(e), "Modification of an Index attribute not permitted")
      exceptionThrown = True
    self.assert_(exceptionThrown)


  def test4(self):
    """ test remove index """
    pcraster.setclone("clone.map")
    exceptionThrown = False
    PlantSpecies = Index.Index([ "Species1", "Species2", "Species3" ])
    try:
      PlantSpecies.__delattr__("Species3")
    except Exception as e:
      self.assertEqual(str(e), "Removal of an Index attribute not permitted")
      exceptionThrown = True
    self.assert_(exceptionThrown)

  def test5(self):
    """ test empty index """
    pcraster.setclone("clone.map")
    exceptionThrown = False
    try:
      PlantSpecies = Index.Index([])
    except Exception as e:
      self.assertEqual(str(e), "Error in initialisation of class Index: no array indices provided")
      exceptionThrown = True
    self.assert_(exceptionThrown)

  def test6(self):
    """ test wrong external names format """
    pcraster.setclone("clone.map")
    exceptionThrown = False
    try:
      PlantSpecies = Index.Index(["TG=Tall=Grass", "SG=ShortGass"])
    except Exception as e:
      self.assertEqual(str(e), "Error in initialisation of class Index: format of TG=Tall=Grass does not match Modelname = Externalname")
      exceptionThrown = True
    self.assert_(exceptionThrown)


  def test7(self):
    """ test external names """
    pcraster.setclone("clone.map")
    PlantSpecies = Index.Index(["TG=TallGrass", "SG=ShortGrass"])
    self.assertEqual(PlantSpecies.__dict__["_values"], ['TG', 'SG'])
    self.assertEqual('TallGrass', PlantSpecies.__dict__["_externalNames"].get("TG"))
    self.assertEqual('ShortGrass', PlantSpecies.__dict__["_externalNames"].get("SG"))

    PlantSpecies = Index.Index(["TG = TallGrass", "SG	=	ShortGrass"])
    self.assertEqual(PlantSpecies.__dict__["_values"], ['TG', 'SG'])
    self.assertEqual('TallGrass', PlantSpecies.__dict__["_externalNames"].get("TG"))
    self.assertEqual('ShortGrass', PlantSpecies.__dict__["_externalNames"].get("SG"))

