#!/usr/bin/env python
# -*- coding: utf-8 -*-
import copy
import testcase
import pcraster
import pcraster.framework
import VariableCollection as vcMod
import Index

class CollectionUnitTests(testcase.TestCase):
  def test1(self):
    """ test setting and modifying default value """
    pcraster.setclone("clone.map")
    PlantSpecies = Index.Index([ "Species1", "Species2", "Species3" ])

    Coll = vcMod.VariableCollection([PlantSpecies], value=7.7)

    self.assertEqual(Coll[PlantSpecies.Species1], 7.7)
    self.assertEqual(Coll[PlantSpecies.Species2], 7.7)
    self.assertEqual(Coll[PlantSpecies.Species3], 7.7)

    Coll[PlantSpecies.Species1] = 1.7
    Coll[PlantSpecies.Species2] = 2.7
    Coll[PlantSpecies.Species3] = 3.7
    self.assertEqual(Coll[PlantSpecies.Species1], 1.7)
    self.assertEqual(Coll[PlantSpecies.Species2], 2.7)
    self.assertEqual(Coll[PlantSpecies.Species3], 3.7)


  def test2(self):
    """ test adding indices """
    pcraster.setclone("clone.map")
    exceptionThrown = False
    PlantSpecies = Index.Index([ "Species1", "Species2", "Species3" ])

    Coll = vcMod.VariableCollection([PlantSpecies], value=None)
    try:
      Coll["Species4"] = 5
    except Exception as e:
      self.assertEqual(str(e), "cannot add elements to a VariableCollection")
      exceptionThrown = True
    self.assert_(exceptionThrown)


  def test3(self):
    """ test looping over indices """
    pcraster.setclone("clone.map")
    PlantSpecies = Index.Index([ "Species3", "Species1", "Species2" ])

    Coll = vcMod.VariableCollection([PlantSpecies], value=0)
    res = ""
    for plant in Coll:
      res += str(plant)
    self.assertEqual(res, "('Species3',)('Species1',)('Species2',)")

  def test4(self):
    """ test assigning new values from other collection """
    pcraster.setclone("clone.map")
    PlantSpecies = Index.Index([ "Species1", "Species2", "Species3" ])

    Coll = vcMod.VariableCollection([PlantSpecies], value=None)
    Coll[PlantSpecies.Species1] = 1.7
    Coll[PlantSpecies.Species2] = 2.7
    Coll[PlantSpecies.Species3] = 3.7

    Coll2 = vcMod.VariableCollection([PlantSpecies], value=0)
    self.assertEqual(Coll2[PlantSpecies.Species1], 0)
    self.assertEqual(Coll2[PlantSpecies.Species2], 0)
    self.assertEqual(Coll2[PlantSpecies.Species3], 0)

    for plant in Coll:
      Coll2[plant] = Coll[plant] + 10

    self.assertEqual(Coll2[PlantSpecies.Species1], 11.7)
    self.assertEqual(Coll2[PlantSpecies.Species2], 12.7)
    self.assertEqual(Coll2[PlantSpecies.Species3], 13.7)

  def test5(self):
    """ test copy collection """
    pcraster.setclone("clone.map")
    PlantSpecies = Index.Index([ "Species1", "Species2", "Species3" ])

    Coll = vcMod.VariableCollection([PlantSpecies], value=None)
    Coll[PlantSpecies.Species1] = 1.7
    Coll[PlantSpecies.Species2] = 2.7
    Coll[PlantSpecies.Species3] = 3.7

    Coll2 = copy.deepcopy(Coll)
    Coll2[PlantSpecies.Species2] = 12.7

    self.assertEqual(Coll[PlantSpecies.Species1], 1.7)
    self.assertEqual(Coll[PlantSpecies.Species2], 2.7)
    self.assertEqual(Coll[PlantSpecies.Species3], 3.7)
    self.assertEqual(Coll2[PlantSpecies.Species1], 1.7)
    self.assertEqual(Coll2[PlantSpecies.Species2], 12.7)
    self.assertEqual(Coll2[PlantSpecies.Species3], 3.7)


  def test6(self):
    """ test copy collection with PCRaster Field objects
    """
    pcraster.setclone("input1.imap")
    Plants = Index.Index([ "TG", "SG"])

    QMax = vcMod.VariableCollection([Plants], value=None)
    QMax[Plants.TG] = pcraster.readmap("input1.imap")
    QMax[Plants.SG] = pcraster.readmap("input2.imap")

    self.assert_(QMax[Plants.TG] is not None)

    value, valid = pcraster.cellvalue(QMax[Plants.TG], 1, 1)
    self.assertAlmostEqual(value, 1.1, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 1, 2)
    self.assertAlmostEqual(value, 3.4 ,6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 1, 3)
    self.assertEqual(valid, False)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 2, 1)
    self.assertAlmostEqual(value, 9.2, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 2, 2)
    self.assertAlmostEqual(value, 1.3, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 2, 3)
    self.assertAlmostEqual(value, 40, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 3, 1)
    self.assertAlmostEqual(value, 20.5, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 3, 2)
    self.assertAlmostEqual(value, 12.4, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 3, 3)
    self.assertAlmostEqual(value, 1.3, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 1, 1)
    self.assertAlmostEqual(value, 2.2, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 1, 2)
    self.assertAlmostEqual(value, 6.8, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 1, 3)
    self.assertAlmostEqual(value, 40, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 2, 1)
    self.assertAlmostEqual(value, 18.4, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 2, 2)
    self.assertAlmostEqual(value, 2.6, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 2, 3)
    self.assertAlmostEqual(value, 80, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 3, 1)
    self.assertAlmostEqual(value, 41, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 3, 2)
    self.assertAlmostEqual(value, 24.8, 5)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 3, 3)
    self.assertAlmostEqual(value, 2.6, 6)

    tmp = copy.deepcopy(QMax)
    self.assert_(tmp[Plants.TG] is not None)

    # should not be the same reference!!! but a fresh copy
    self.assert_(tmp[Plants.TG] is not QMax[Plants.TG] )

    # tmp values should equal qmax
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 1, 1)
    self.assertAlmostEqual(value, 1.1, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 1, 2)
    self.assertAlmostEqual(value, 3.4, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 1, 3)
    self.assertEqual(valid, False)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 2, 1)
    self.assertAlmostEqual(value, 9.2, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 2, 2)
    self.assertAlmostEqual(value, 1.3)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 2, 3)
    self.assertAlmostEqual(value, 40)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 3, 1)
    self.assertAlmostEqual(value, 20.5)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 3, 2)
    self.assertAlmostEqual(value, 12.4, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 3, 3)
    self.assertAlmostEqual(value, 1.3)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 1, 1)
    self.assertAlmostEqual(value, 2.2)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 1, 2)
    self.assertAlmostEqual(value, 6.8, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 1, 3)
    self.assertAlmostEqual(value, 40)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 2, 1)
    self.assertAlmostEqual(value, 18.4, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 2, 2)
    self.assertAlmostEqual(value, 2.6, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 2, 3)
    self.assertAlmostEqual(value, 80)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 3, 1)
    self.assertAlmostEqual(value, 41)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 3, 2)
    self.assertAlmostEqual(value, 24.8, 5)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 3, 3)
    self.assertAlmostEqual(value, 2.6, 6)

    tmp[Plants.TG] = tmp[Plants.TG] * 3.0
    tmp[Plants.SG] = tmp[Plants.SG] + 2.0

    # no modification qmax
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 1, 1)
    self.assertAlmostEqual(value, 1.1)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 1, 2)
    self.assertAlmostEqual(value, 3.4, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 1, 3)
    self.assertEqual(valid, False)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 2, 1)
    self.assertAlmostEqual(value, 9.2, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 2, 2)
    self.assertAlmostEqual(value, 1.3)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 2, 3)
    self.assertAlmostEqual(value, 40)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 3, 1)
    self.assertAlmostEqual(value, 20.5)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 3, 2)
    self.assertAlmostEqual(value, 12.4, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.TG], 3, 3)
    self.assertAlmostEqual(value, 1.3)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 1, 1)
    self.assertAlmostEqual(value, 2.2)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 1, 2)
    self.assertAlmostEqual(value, 6.8, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 1, 3)
    self.assertAlmostEqual(value, 40)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 2, 1)
    self.assertAlmostEqual(value, 18.4, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 2, 2)
    self.assertAlmostEqual(value, 2.6, 6)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 2, 3)
    self.assertAlmostEqual(value, 80)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 3, 1)
    self.assertAlmostEqual(value, 41)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 3, 2)
    self.assertAlmostEqual(value, 24.8, 5)
    value, valid = pcraster.cellvalue(QMax[Plants.SG], 3, 3)
    self.assertAlmostEqual(value, 2.6, 6)

    # tmp should be modified
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 1, 1)
    self.assertAlmostEqual(value, 3.3, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 1, 2)
    self.assertAlmostEqual(value, 10.2, 5)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 1, 3)
    self.assertEqual(valid, False)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 2, 1)
    self.assertAlmostEqual(value, 27.6, 5)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 2, 2)
    self.assertAlmostEqual(value, 3.9, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 2, 3)
    self.assertAlmostEqual(value, 120, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 3, 1)
    self.assertAlmostEqual(value, 61.5)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 3, 2)
    self.assertAlmostEqual(value, 37.2, 5)
    value, valid = pcraster.cellvalue(tmp[Plants.TG], 3, 3)
    self.assertAlmostEqual(value, 3.9, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 1, 1)
    self.assertAlmostEqual(value, 4.2, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 1, 2)
    self.assertAlmostEqual(value, 8.8, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 1, 3)
    self.assertAlmostEqual(value, 42, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 2, 1)
    self.assertAlmostEqual(value, 20.4, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 2, 2)
    self.assertAlmostEqual(value, 4.6, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 2, 3)
    self.assertAlmostEqual(value, 82, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 3, 1)
    self.assertAlmostEqual(value, 43, 6)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 3, 2)
    self.assertAlmostEqual(value, 26.8, 5)
    value, valid = pcraster.cellvalue(tmp[Plants.SG], 3, 3)
    self.assertAlmostEqual(value, 4.6, 6)



  def test7(self):
    """ test reading some values from disk (1dim) """
    pcraster.setclone("clone.map")
    Plants = Index.Index([ "TG", "SG"])

    QMax = vcMod.VariableCollection([Plants], value=vcMod.ValueFromParameterTable("QMax", "parameterFile.tbl", pcraster.Scalar))
    self.assertEqual(QMax[Plants.TG], 12000)
    self.assertEqual(QMax[Plants.SG], 18000)

    Cvr = vcMod.VariableCollection([Plants], value=vcMod.ValueFromParameterTable("Cvr", "parameterFile.tbl", pcraster.Scalar))

    self.assert_(isinstance(Cvr[Plants.TG], pcraster._pcraster.Field))
    self.assert_(isinstance(Cvr[Plants.SG], pcraster._pcraster.Field))

    kv = vcMod.VariableCollection([Plants], value=vcMod.ValueFromParameterTable("kv", "parameterFile.tbl", pcraster.Nominal))
    self.assertEqual(kv[Plants.TG], 3)
    self.assertEqual(kv[Plants.SG], 7)


  def test8(self):
    """ test multiple initialisation of values """
    pcraster.setclone("clone.map")
    exceptionThrown = False
    Plants = Index.Index([ "TG", "SG"])

    try:
      QMax1 = vcMod.VariableCollection([Plants], value=vcMod.ValueFromParameterTable("QMax1", "parameterFile.tbl", pcraster.Scalar))
    except ValueError as e:
      self.assertEqual(str(e), "Error reading parameterFile.tbl line 21, QMax1 'TG' already initialised")
      exceptionThrown = True
    self.assert_(exceptionThrown)

  def test9(self):
    """ test initialisation with non-existing key """
    pcraster.setclone("clone.map")
    exceptionThrown = False
    Plants = Index.Index([ "TG", "SG"])

    try:
      QMax2 = vcMod.VariableCollection([Plants], value=vcMod.ValueFromParameterTable("QMax2", "parameterFile.tbl", pcraster.Scalar))
    except ValueError as e:
      self.assertEqual(str(e), "Error reading parameterFile.tbl line 26, 'sG' unknown collection index")
      exceptionThrown = True
    self.assert_(exceptionThrown)


  def test10(self):
    """ test reading values from disk (2dim) """
    pcraster.setclone("clone.map")
    Plants = Index.Index(["P1", "P2", "P3", "P4"] )
    Herbivores = Index.Index(["SpA", "SpB", "SpC"] )
    Interaction = vcMod.VariableCollection([Herbivores, Plants], value=vcMod.ValueFromParameterTable("Interaction", "parameterFile.tbl", pcraster.Scalar))
    self.assertEqual(Interaction[Herbivores.SpA, Plants.P1], 0.1)
    self.assertEqual(Interaction[Herbivores.SpA, Plants.P2], 0.11)
    self.assertEqual(Interaction[Herbivores.SpA, Plants.P3], 0.12)
    self.assertEqual(Interaction[Herbivores.SpA, Plants.P4], 0.14)
    self.assertEqual(Interaction[Herbivores.SpB, Plants.P1], 0.23)
    self.assertEqual(Interaction[Herbivores.SpB, Plants.P2], 0.24)
    self.assertEqual(Interaction[Herbivores.SpB, Plants.P3], 0.25)
    self.assertEqual(Interaction[Herbivores.SpB, Plants.P4], 0.26)
    self.assertEqual(Interaction[Herbivores.SpC, Plants.P1], 0.34)
    self.assertEqual(Interaction[Herbivores.SpC, Plants.P2], 0.35)
    self.assertEqual(Interaction[Herbivores.SpC, Plants.P3], 0.36)
    self.assertEqual(Interaction[Herbivores.SpC, Plants.P4], 0.37)



  def test11(self):
    """ test reading values from disk with external names """
    pcraster.setclone("clone.map")
    Plants = Index.Index(["TG=TallGrass", "SG=ShortGrass"] )
    PlantsAvailable = vcMod.VariableCollection([Plants], value=vcMod.ValueFromParameterTable("PlantsAvailable", "parameterFile.tbl", pcraster.Scalar))
    self.assertEqual(PlantsAvailable[Plants.TG], 1.3)
    self.assertEqual(PlantsAvailable[Plants.SG] , 4.5)

    Herbivores = Index.Index(["Cow=CowWithLongName", "Horse=HorseWithLongName", "Sheep=SheepWithLongName"] )
    InteractionExt = vcMod.VariableCollection([Herbivores, Plants], value=vcMod.ValueFromParameterTable("InteractionExt", "parameterFile.tbl", pcraster.Scalar))
    self.assertEqual(InteractionExt[Herbivores.Cow, Plants.TG] , 0.6)
    self.assertEqual(InteractionExt[Herbivores.Cow, Plants.SG] , 0.67)
    self.assertEqual(InteractionExt[Herbivores.Horse, Plants.TG] , 0.73)
    self.assertEqual(InteractionExt[Herbivores.Horse, Plants.SG] , 0.74)
    self.assertEqual(InteractionExt[Herbivores.Sheep, Plants.TG ], 0.87)
    self.assertEqual(InteractionExt[Herbivores.Sheep, Plants.SG] , 0.89)

  def test12(self):
    """ test looping with tuple """
    pcraster.setclone("clone.map")
    Plants = Index.Index(["TG=TallGrass", "SG=ShortGrass"])

    Herbivores = Index.Index(["Cow=CowWithLongName", "Horse=HorseWithLongName", "Sheep=SheepWithLongName"] )
    InteractionExt = vcMod.VariableCollection([Herbivores, Plants], value=vcMod.ValueFromParameterTable("InteractionExt", "parameterFile.tbl", pcraster.Scalar))
    for h,p in InteractionExt:
      InteractionExt[h,p] *= 10
    self.assertEqual(InteractionExt[Herbivores.Cow, Plants.TG],   6.0)
    # TODO fails self.assertAlmostEqual(InteractionExt[Herbivores.Cow, Plants.SG], 6.7, 6)
    self.assertEqual(InteractionExt[Herbivores.Horse, Plants.TG], 7.3)
    self.assertEqual(InteractionExt[Herbivores.Horse, Plants.SG], 7.4)
    self.assertEqual(InteractionExt[Herbivores.Sheep, Plants.TG], 8.7)
    self.assertEqual(InteractionExt[Herbivores.Sheep, Plants.SG], 8.9)

  #def test13(self):
    #""" initialize with a callable """
    #self.assert_(False, "create this test")

    #class TssCreator(object):
      #def __init__(self, dynamicClass):
        #self._dynamicClass = dynamicClass
        ## TODO
        ## try the Metaclasses example on page 381
        ## to check dynamicClass has the method we are going
        ## to call within the TimeoutputTimeseries code.

      #def defaultValue(self, varName, indexTuple):
        #tss = varName+"-"+"-".join(indexTuple)
        #return TimeoutputTimeseries(self._dynamicClass, tss, idMap=None, noHeader=False)

    #Plants = Index.Index(["TG=TallGrass", "SG=ShortGrass"] )
    #Herbivores = Index.Index(["Cow=CowWithLongName", "Horse=HorseWithLongName", "Sheep=SheepWithLongName"] )
    #tssCreator = TssCreator(selfInTheRealCodeThatIsADynamicFrameworkClass)
    #IE = vcMod.VariableCollection([Herbivores, Plants], defaultValue= tssCreator)
