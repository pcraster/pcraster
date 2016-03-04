#!/usr/bin/env python
# -*- coding: utf-8 -*-
from pcraster import *
from pcraster.framework import *
from pcraster.collection import *
import random

class ParameterFileModel(object):
  def __init__(self, cloneMap):
    setclone(cloneMap)

  def initial(self):

    # Plants  = [TG,SG];
    self.Plants = Index(["TG", "SG"])

    # initialising QMax[Plants]
    self.QMax = VariableCollection([self.Plants], value=ValueFromParameterTable("QMax", "plant.tbl", Scalar))

    # initialising Cvr[Plants]
    self.Cvr = VariableCollection([self.Plants], value=ValueFromParameterTable("Cvr", "plant.tbl", Scalar))

    #foreach p in Plants {
    #  dH[p]          = 0;
    #  dH_1[p]        = 0;
    #}
    self.dH = VariableCollection([self.Plants], value=0)
    self.dH_1 = VariableCollection([self.Plants], value=0)

    # initialising timeseries report
    self.CvrTss =VariableCollection([self.Plants], value=ValueTimeoutputTimeseries("Cvr", self, idMap="clone.cln", noHeader=False))

  def dynamic(self):
    # just some random calculations
    for plant in self.Plants:
      self.dH[plant] = random.random()
      self.dH_1[plant] += self.dH[plant]
      self.Cvr[plant] += self.dH_1[plant]

      # reporting timeseries
      self.CvrTss[plant].sample(self.Cvr[plant])


model = ParameterFileModel("clone.cln")
dynModel = DynamicFramework(model, endTimeStep=5, firstTimestep=1)
dynModel.run()
