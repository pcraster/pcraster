#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pcraster import *
import pcraster.framework.dynamicPCRasterBase as dynamicPCRasterBase


#

#
class T1(dynamicPCRasterBase.DynamicModel):
  def __init__(self):
    dynamicPCRasterBase.DynamicModel.__init__(self)
    setclone("clone.map")



#
class T2(dynamicPCRasterBase.DynamicModel):
  def __init__(self):
    dynamicPCRasterBase.DynamicModel.__init__(self)
    setclone("clone.map")
    self.value = 0
    self.initialValue = 0

  def initial(self):
    self.value += 1
    self.initialValue += 1

  def dynamic(self):
    self.value += 1


#
class TestStartTime(dynamicPCRasterBase.DynamicModel):
  def __init__(self):
    dynamicPCRasterBase.DynamicModel.__init__(self)
    setclone("clone.map")

  def initial(self):
    self.initValue = 2
    self.value = 0

  def dynamic(self):
    self.value += cellvalue(timeinputscalar("timeseries.tss", "clone.map"), 1)[0]


class TestReadmapReport(dynamicPCRasterBase.DynamicModel):
  def __init__(self):
    dynamicPCRasterBase.DynamicModel.__init__(self)
    setclone("clone.map")

  def initial(self):
    self.raster = self.readmap("plus")
    #
    value, isValid = cellvalue(self.raster, 1)
    assert isValid == False
    #
    value, isValid = cellvalue(self.raster, 2)
    assert isValid == True
    assert value == 8
    #
    value, isValid = cellvalue(self.raster, 3)
    assert isValid == True
    assert value == 2
    #
    value, isValid = cellvalue(self.raster, 4)
    assert isValid == True
    assert value == 2
    #
    value, isValid = cellvalue(self.raster, 5)
    assert isValid == False
    #
    value, isValid = cellvalue(self.raster, 6)
    assert isValid == True
    assert value == -6
    #
    value, isValid = cellvalue(self.raster, 7)
    assert isValid == True
    assert value == 100
    #
    value, isValid = cellvalue(self.raster, 8)
    assert isValid == True
    assert value == -7
    #
    value, isValid = cellvalue(self.raster, 9)
    assert isValid == True
    assert value == 16

    self.report(self.raster, "static")

  def dynamic(self):
    self.report(self.raster, "dyna")

