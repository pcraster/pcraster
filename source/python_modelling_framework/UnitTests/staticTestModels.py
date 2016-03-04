#!/usr/bin/env python
# -*- coding: utf-8 -*-
import pcraster
import pcraster.framework.staticPCRasterBase as staticPCRasterBase


class T1(staticPCRasterBase.StaticModel):
  def __init__(self):
    staticPCRasterBase.StaticModel.__init__(self)
    pass


class T2(staticPCRasterBase.StaticModel):
  def __init__(self):
    staticPCRasterBase.StaticModel.__init__(self)
    self.value = 0

  def initial(self):
    self.value += 1


class ReadmapReport(staticPCRasterBase.StaticModel):
  def __init__(self):
    staticPCRasterBase.StaticModel.__init__(self)
    #pass

  def initial(self):
    raster = self.readmap("plus")
    #
    value, isValid = pcraster.cellvalue(raster, 1)
    assert isValid == False
    #
    value, isValid = pcraster.cellvalue(raster, 2)
    assert isValid == True
    assert value == 8
    #
    value, isValid = pcraster.cellvalue(raster, 3)
    assert isValid == True
    assert value == 2
    #
    value, isValid = pcraster.cellvalue(raster, 4)
    assert isValid == True
    assert value == 2
    #
    value, isValid = pcraster.cellvalue(raster, 5)
    assert isValid == False
    #
    value, isValid = pcraster.cellvalue(raster, 6)
    assert isValid == True
    assert value == -6
    #
    value, isValid = pcraster.cellvalue(raster, 7)
    assert isValid == True
    assert value == 100
    #
    value, isValid = pcraster.cellvalue(raster, 8)
    assert isValid == True
    assert value == -7
    #
    value, isValid = pcraster.cellvalue(raster, 9)
    assert isValid == True
    assert value == 16

    self.report(raster, "static")



class Static1(staticPCRasterBase.StaticModel):
  def __init__(self):
    pass


