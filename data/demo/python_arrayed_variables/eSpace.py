#!/usr/bin/env python
# -*- coding: utf-8 -*-

# NEW DEMO dd 27 oct 2009 and on

from pcraster import *
from pcraster.framework import *
from pcraster.collection import *
import math

class ESpace(object):
  def __init__(self, cloneMap):
    setclone(cloneMap)
    setrandomseed(1)

  def initial(self):
    self.tDay   = 86400.  # [s d-1]

    # [-] increase of activities, relative to maintenance cost:
    self.ct   = 1.54  # travelling
    self.cc   = 1.54  # cropping
    self.ch   = 1.54  # handling
    self.cd   = 1.24  # digesting

    self.CMnt = 3.4   # [J kg-0.75 s-1] maintenance
    self.CRsp = 30000 # [J g-1] respiration
    self.CGrw = 37000 # [J g-1] growth

    self.thMin   = 0.86 # [s bite-1]  {Gross et al. 1993, Tb. 2}

    self.Plants = Index(["TG", "SG"] )

    # was in original, but never used
    # self.Cvr = self.initializedPlantsVar("Cvr", dataType=None)

    self.QMax = self.initializedPlantsVar("QMax")
    self.kv = self.initializedPlantsVar("kv")


    self.rv = self.initializedPlantsVar("rv")
    self.fLeaf2Stalk = self.initializedPlantsVar("fLeaf2Stalk")
    self.AvDstPatchMin = self.initializedPlantsVar("AvDstPatchMin")
    self.AvDstPatchMax = self.initializedPlantsVar("AvDstPatchMax")
    self.StdevDstPatchMin = self.initializedPlantsVar("StdevDstPatchMin")
    self.StdevDstPatchMax = self.initializedPlantsVar("StdevDstPatchMax")
    self.Dim = self.initializedPlantsVar("Dim")

    self.w = 100 # [kg] herbivores body mass
    self.w_m = self.w**0.75  # [kg^0.75] metabolic mass
    cRC = 23 # [g kg-1] rumen content per unit body mass
    self.fRCmin = 0.75 # [-] fraction of rumen content depletion to restart feeding
    self.RCMax = cRC*self.w # [g] maximum rumen content to stop feeding {AHE, p}
    self.RCMin = self.fRCmin * self.RCMax # [g] minimum food in rumen to restart feeding (=0.75 for 3-h feeding bouts)
    self.k = -0.1/3600 # s-1 turnover rate of rumen content

    cbite = 0.003 # [g kg-1] bite mass per unit body mass {estimated from Gross et al., 1993,table 4}
    self.bm = cbite*self.w # [g] bite mass
    self.vt = 0.50*self.w**0.13 # [m s-1] traveling speed, increases by 2cm per kg animal mass {Calder 1983}
    self.vc = 0.5*self.vt # [m s-1] cropping speed

    aMin  = 0.05 # [-] minimum fraction of vegetation removed by a herbivore while visiting a patch
    aMax  = 0.25 # [-] maximum fraction of vegetation removed by a herbivore while visiting a patch

    self.dH = self.plantsStateVar(0.)
    self.dH_max = self.plantsStateVar(0.)

    self.dHdV = self.plantsStateVar(0.)
    self.dHdV_max = self.plantsStateVar(0.)

    self.V = self.plantsStateVar(5.)
    self.V1_star = self.plantsStateVar(0.)
    self.V2_star = self.plantsStateVar(0.)
    self.V_max = self.plantsStateVar(0.)

    self.DnsHrbv_b = self.plantsStateVar(0.)
    self.DnsH1_star_b = self.plantsStateVar(0.)
    self.DnsH2_star_b = self.plantsStateVar(0.)

    self.DnsHrbv_n = self.plantsStateVar(0.)
    self.DnsH1_star_n = self.plantsStateVar(0.)
    self.DnsH2_star_n = self.plantsStateVar(0.)

    self.Util = self.plantsStateVar(0.)
    self.U1_star = self.plantsStateVar(0.)
    self.U2_star = self.plantsStateVar(0.)

    # not used
    # self.Ch_rumen = self.plantsStateVar(0.)
    # self.dUdV = self.plantsStateVar(0.)

    self.aTss = self.timeseriesCollection("a")
    self.a_minTss = self.timeseriesCollection("a_min")
    self.a_maxTss = self.timeseriesCollection("a_max")
    self.VTss = self.timeseriesCollection("V")
    self.V_low_starTss = self.timeseriesCollection("V_low_star")
    self.V_high_starTss = self.timeseriesCollection("V_high_star")
    self.V_maxTss = self.timeseriesCollection("V_max")
    self.VrelTss = self.timeseriesCollection("Vrel")
    self.QTss = self.timeseriesCollection("Q")
    self.QRedTss = self.timeseriesCollection("QRed")
    self.In_bTss = self.timeseriesCollection("In_b")
    self.In_eTss = self.timeseriesCollection("In_e")
    # per Bite
    self.Dst_biteTss = self.timeseriesCollection("Dst_bite")
    self.DstLf_biteTss = self.timeseriesCollection("DstLf_bite")
    self.DstSt_biteTss = self.timeseriesCollection("DstSt_bite")
    self.Dns_biteTss = self.timeseriesCollection("Dns_bite")
    self.DnsLf_biteTss = self.timeseriesCollection("DnsLf_bite")
    self.DnsSt_biteTss = self.timeseriesCollection("DnsSt_bite")
    # per Patch
    self.Cc_patchTss = self.timeseriesCollection("Cc_patch")
    self.Cd_patchTss = self.timeseriesCollection("Cd_patch")
    self.Ch_patchTss = self.timeseriesCollection("Ch_patch")
    self.Ct_patchTss = self.timeseriesCollection("Ct_patch")
    self.tc_patchTss = self.timeseriesCollection("tc_patch")
    self.td_patchTss = self.timeseriesCollection("td_patch")
    self.th_patchTss = self.timeseriesCollection("th_patch")
    self.tt_patchTss = self.timeseriesCollection("tt_patch")
    self.Dst_patchTss = self.timeseriesCollection("Dst_patch")
    self.NPatch_patchTss = self.timeseriesCollection("NPatch_patch")
    # per rumen fill
    self.tc_rumenTss = self.timeseriesCollection("tc_rumen")
    self.td_rumenTss = self.timeseriesCollection("td_rumen")
    self.th_rumenTss = self.timeseriesCollection("th_rumen")
    self.tt_rumenTss = self.timeseriesCollection("tt_rumen")
    # per Day
    self.Cc_dayTss = self.timeseriesCollection("Cc_day")
    self.Cd_dayTss = self.timeseriesCollection("Cd_day")
    self.Ch_dayTss = self.timeseriesCollection("Ch_day")
    self.Ct_dayTss = self.timeseriesCollection("Ct_day")
    self.tc_dayTss = self.timeseriesCollection("tc_day")
    self.td_dayTss = self.timeseriesCollection("td_day")
    self.th_dayTss = self.timeseriesCollection("th_day")
    self.tt_dayTss = self.timeseriesCollection("tt_day")
    self.tbout_dayTss = self.timeseriesCollection("tbout_day")
    self.Nbout_dayTss = self.timeseriesCollection("Nbout_day")
    # herbivore output
    self.RspTss = self.timeseriesCollection("Rsp")
    self.GrwTss = self.timeseriesCollection("Grw")
    self.dHTss = self.timeseriesCollection("dH")
    self.dH_maxTss = self.timeseriesCollection("dH_max")
    self.dH_max_relTss = self.timeseriesCollection("dH_max_rel")
    self.DnsHrbv_massTss = self.timeseriesCollection("DnsHrbv_mass")
    self.DnsHrbv_numTss = self.timeseriesCollection("DnsHrbv_num")
    self.UtilTss = self.timeseriesCollection("Util")
    self.DnsH_low_star_massTss = self.timeseriesCollection("DnsH_low_star_mass")
    self.DnsH_high_star_massTss = self.timeseriesCollection("DnsH_high_star_mass")
    self.DnsH_low_star_numTss = self.timeseriesCollection("DnsH_low_star_num")
    self.DnsH_high_star_numTss = self.timeseriesCollection("DnsH_high_star_num")
    self.U_low_starTss = self.timeseriesCollection("U_low_star")
    self.U_high_starTss = self.timeseriesCollection("U_high_star")


  def initializedPlantsVar(self, name, dataType=Scalar):
    """create a VariableCollection with Plants index
       and initialize its value from plant.tbl"""
    return VariableCollection([self.Plants],
            value=ValueFromParameterTable(name, "plant.tbl", dataType))

  def plantsStateVar(self, value):
    return VariableCollection([self.Plants], value)

  def timeseriesCollection(self, variableName):
    """
    returns collection of timeseries objects
    """
    # for output purpose if some experimental design is used using a map. then ouput for each gridcell (see timeoutput in report section).
    classes = nominal(1)
    return VariableCollection([self.Plants], value=ValueTimeoutputTimeseries(variableName, self, idMap=classes, noHeader=False))


  def dynamic(self):
    for p in self.Plants:
      ## store values of previous time step for linear interpolation at V*

      # CW assignment before right side use, no need for arraying the var
      dHdV_1       = self.dHdV[p]
      V_1          = self.V[p]
      dH_1         = self.dH[p]

      DnsH_b_1     = self.DnsHrbv_b[p]
      DnsH_n_1     = self.DnsHrbv_n[p]
      U_1          = self.Util[p]

      # standing biomass [g m-2] outcommented lines are used for different scenarios
      #  self.V[p] = 10*time()   # linear increase of vegetation density with time

      # not used
      # VMax = 5*self.nrTimeSteps()

      VRel = self.V[p]/self.kv[p] # switched on if vegetation density is simulated according to logistic model

      # a = min(aMax, max(aMin, aMax + (aMin-aMax)*VRel[p])) # acceptance imposed between [0.05 - 0.25]
      a = self.rv[p]/2 # optimal acceptance in coupled herbivore - vegetation model

      QRed = 1.-(1-self.fLeaf2Stalk[p])*VRel # reduction of quality, proportional to stalk fraction

      # available, acceptable and accessable energy [J m-2]
      Q = self.QMax[p] * QRed  # [J g-1] conversion of DM to energy
      E_a = a * Q * self.V[p] # [J m-2]

      # travelling time and �costs between patches (1m2 each)
      #  spatially explicit: determine distance between animal at (x,y) and all patches in the landscape
      #  IsPatch     = (MeanPatch + normal(1)*StdevPatch) >= (1/(StdevPatch*sqrt(2*math.pi))) * exp(-(Dist2Loc-MeanPatch)**2/(2*StdevPatch**2))
      #  XPatch      = if (IsPatch then xcoordinate(IsPatch) else scalar(0))      # x-coordinate of animal in landscape
      #  YPatch      = if (IsPatch then ycoordinate(IsPatch) else scalar(0))      # y-coordinate of animal in landscape
      #  XDst        = (XPatch-X)*(XPatch-X)
      #  YDst        = (YPatch-Y)*(YPatch-Y)
      #  DstSqr      = XDst-YDst
      #  Dp          = sqrt(DstSqr)    # non-zero distances only otherwise missing values, not taken into account in landscape average

      #   spatically implicit: use fixed inter-patch distance, or generate variable inter-patch distance depending of vegetation density of a patch
      AvDstPatch   = self.AvDstPatchMax[p]  + (self.AvDstPatchMin[p]-self.AvDstPatchMax[p])  * VRel   # patch inter-distance varied with relative vegetation density
      StdevDstPatch= self.StdevDstPatchMax[p] + (self.StdevDstPatchMin[p]-self.StdevDstPatchMax[p])* VRel # variance in patch interdistance (usually set to zero in PLANT.TBL
      Dp           = max(0.,AvDstPatch + mapnormal()*StdevDstPatch)   # generated patch inter-distance
      tt_patch     = Dp / self.vt
      Ct_patch     = tt_patch * self.ct * self.CMnt * self.w_m

      # cropping time and -costs per bite and m2
      Dns_bite     = self.V[p] / self.bm
      Dns_leaf     = (self.V[p]*self.fLeaf2Stalk[p])/self.bm
      Dns_stalk    = (self.V[p]*(1-self.fLeaf2Stalk[p]))/self.bm

      MeanDstBite  = 1 / (Dns_bite**(1/self.Dim[p]))
      MeanDstLeaf  = 1 / (Dns_leaf**(1/self.Dim[p]))
      MeanDstStalk = 1 / (Dns_stalk**(1/self.Dim[p]))

      Db           = MeanDstBite
      tc_bite      = Db / self.vc
      Cc_bite      = tc_bite * self.cc * self.CMnt * self.w_m
      tc_patch     = a * tc_bite * Dns_bite
      Cc_patch     = tc_patch * self.cc * self.CMnt * self.w_m

     # handling time and -costs per bite and m2
      th_bite      = self.thMin / QRed
      Ch_bite      = th_bite * self.ch * self.CMnt *self. w_m
      th_patch     = a * th_bite * Dns_bite
      Ch_patch     = th_patch * self.ch * self.CMnt * self.w_m

    # digesting time and cost, per rumen fill and m2
      NPatch       = (self.RCMax - self.RCMin) / (a*self.V[p])       # number of patches required to fill up rumen to maximum level
      td_rumen     = max(0.,ln(self.fRCmin)/(self.k*QRed))      # [s] time required to digest present food in rumen to minimum level
      Cd_rumen     = td_rumen * self.cd * self.CMnt * self.w_m
      td_patch     = td_rumen / NPatch
      Cd_patch     = Cd_rumen / NPatch

      tt_rumen     = tt_patch*NPatch
      tc_rumen     = tc_patch*NPatch
      th_rumen     = th_patch*NPatch

      # CW never used in the model
      # Ct_rumen = tt_rumen * self.ct * self.CMnt * self.w_m
      # Cc_rumen     = tc_rumen * self.cc * self.CMnt * self.w_m
      # self.Ch_rumen[p]     = th_rumen * self.ch * self.CMnt * self.w_m

      t_bout       = tt_rumen + tc_rumen + th_rumen + td_rumen # a 'bout' is a full cycle of travelling, cropping, handling, digestion for 1 full rumen
      N_bout       = self.tDay / t_bout

      E_d          = Ct_patch + Cc_patch + Ch_patch + Cd_patch
      E_i          = E_a - E_d
      g            = 1 / (tt_patch + tc_patch + th_patch + td_patch)  # [s-1]
      In_e         = g*E_i # [J s-1 m-2] instantaneous energy intake rate
      In_b         = a * self.V[p] * NPatch * N_bout    # [g d-1]     daily DM intake rate

      tt_day       = N_bout * tt_rumen
      tc_day       = N_bout * tc_rumen
      th_day       = N_bout * th_rumen
      td_day       = N_bout * td_rumen

      Ct_day       = tt_day * self.ct * self.CMnt * self.w_m
      Cc_day       = tc_day * self.cc * self.CMnt * self.w_m
      Ch_day       = th_day * self.ch * self.CMnt * self.w_m
      Cd_day       = td_day * self.cd * self.CMnt * self.w_m

      Rsp          = (Ct_day + Cc_day + Ch_day + Cd_day) / self.CRsp   # [g d-1 animal-1]  energy costs for  maintenance respiration
      Grw          = (N_bout * NPatch * E_a) / self.CGrw# [g d-1 animal-1]  energy costs for growth respiration

      # CW feedback/state used earlier before redefinition
      self.dH[p]           = Grw - Rsp    # [g d-1 animal-1]  no integration of herbivore body mass. we are rather only interested in the implications of animals body mass on intake response curve
      dV           = self.rv[p]*self.V[p]*(1-self.V[p]/self.kv[p])     # [g d-1 m-2]
      # CW feedback/state, incrementing it
      self.V[p]            = self.V[p] + dV # [g m-2]           only imposed vegetation density, to see the effect on intake rate

  # ---------- DERIVED STATISTICS FOR OUTPUT PURPOSE ONLY

      diffV = ifthenelse((abs(self.V[p]-V_1) > scalar(0)), (self.V[p]-V_1), scalar(1.E-6))# =dV, just to guarantee that proper values are used in case vegetation density model is changed
      self.dHdV[p]         = (self.dH[p]-dH_1)/diffV

      # never used
      # a_HV         = self.rv[p]/2

      a_min = min(1,max(0,Ct_patch / (Q*self.V[p])))
      a_max = min(1,max(0,E_d / (Q*self.V[p])))

      self.DnsHrbv_b[p]    = self.w / NPatch    # [g m-2] herbivore mass-density
      self.DnsHrbv_n[p]    = self.DnsHrbv_b[p]/self.w   # [number per 100 ha] herbivore numeric density
      self.Util[p]         = self.DnsHrbv_b[p]/self.V[p]# [g g-1] utilization herbivore mass / plant mass


      # never used
      # self.dUdV[p]         = (self.Util_1[p]-self.Util[p])/diffV

      # NB1:  1_ in variable names refers to statistics to V at low value of V associated with zero herbivore growth 2_ refers to high  value of V associated with zero herbivore growth
      # NB2: _1 refers to value of variable in previous time step

      # use linear interpolation (Y = c1*x + c2) to determine V1* and derives statistics at V1*. c1 and c2 are dummy variables to store intermediate results

      # CW common term
      dHFromNegToPos = dH_1 < 0  & self.dH[p] > 0

      # CW helper function
      def c12(self, term1, term2):
        c1 = (term1-term2)/diffV
        c2 = term2-c1*self.V[p]
        return c1, c2

      # note, no class function (e.g. self.c12(...))
      c1, c2 = c12(self, dH_1, self.dH[p])
      self.V1_star[p] = ifthenelse (dHFromNegToPos, -c2/c1, self.V1_star[p])# [g m-2] at dH=0

      def YisV1_star(self, term1, term2):
        c1, c2 = c12(self, term1, term2)
        return self.V1_star[p]*c1+c2

      y = YisV1_star(self, U_1, self.Util[p])
      self.U1_star[p] = ifthenelse(dHFromNegToPos, y, self.U1_star[p]) # [g g-1] at dH=0

      y = YisV1_star(self, DnsH_b_1, self.DnsHrbv_b[p])
      self.DnsH1_star_b[p] = ifthenelse(dHFromNegToPos, y, self.DnsH1_star_b[p]) # [g m-2] at dH=0

      y = YisV1_star(self, DnsH_n_1,  self.DnsHrbv_n[p])
      self.DnsH1_star_n[p] = ifthenelse(dHFromNegToPos, y, self.DnsH1_star_n[p]) # [100ha-1] at dH=0]

       # use linear interpolation (Y = c1*x + c2) to determine V2* and derives statistics at V2*
      c1, c2 = c12(self, dH_1, self.dH[p])
      self.V2_star[p] = ifthenelse(dHFromNegToPos, -c2/c1, self.V2_star[p]) # [g m-2] at dH=0

      def YisV2_star(self, term1, term2):
        c1, c2 = c12(self, term1, term2)
        return self.V2_star[p]*c1+c2

      y = YisV2_star(self, U_1, self.Util[p])
      self.U2_star[p] = ifthenelse(dHFromNegToPos, y, self.U2_star[p]) # [g g-1] at dH=0

      y = YisV2_star(self, DnsH_b_1, self.DnsHrbv_b[p])
      self.DnsH2_star_b[p] = ifthenelse (dHFromNegToPos, y, self.DnsH2_star_b[p]) # [g m-2] at dH=0

      y = YisV2_star(self, DnsH_n_1, self.DnsHrbv_n[p])
      self.DnsH2_star_n[p] = ifthenelse(dHFromNegToPos, y, self.DnsH2_star_n[p]) # [100ha-1] at dH=0]

      # use linear interpolation to determine V_max and dH_max
      self.V_max[p] = ifthenelse (dHFromNegToPos, (V_1+self.V[p])/2, self.V_max[p])
      self.dH_max[p] = ifthenelse (dHFromNegToPos, (dH_1+self.dH[p])/2, self.dH_max[p])
      dH_max_rel = self.dH_max[p]/self.w

      # REPORT SECTION

      # CW example how to report a map stack
      # with the result do: aguila --timesteps=[1,50] V-SG- V-TG-
      self.report(self.V[p],"V-%s-" % p)

      # available biomass,

      self.aTss[p].sample(a)
      self.a_minTss[p].sample(a_min)
      self.a_maxTss[p].sample(a_max)

      self.VTss[p].sample(self.V[p])
      self.V_low_starTss[p].sample(self.V1_star[p])
      self.V_high_starTss[p].sample(self.V2_star[p])
      self.V_maxTss[p].sample(self.V_max[p])

      self.VrelTss[p].sample(VRel)
      self.QTss[p].sample(Q)
      self.QRedTss[p].sample(QRed)

      self.In_bTss[p].sample(In_b)
      self.In_eTss[p].sample(In_e)

      # per Bite
      self.Dst_biteTss[p].sample(MeanDstBite)
      self.DstLf_biteTss[p].sample(MeanDstLeaf)
      self.DstSt_biteTss[p].sample(MeanDstStalk)

      self.Dns_biteTss[p].sample(Dns_bite)
      self.DnsLf_biteTss[p].sample(Dns_leaf)
      self.DnsSt_biteTss[p].sample(Dns_stalk)

      # per Patch
      self.Cc_patchTss[p].sample(Cc_patch*1.E-6)
      self.Cd_patchTss[p].sample(Cd_patch*1.E-6)
      self.Ch_patchTss[p].sample(Ch_patch*1.E-6)
      self.Ct_patchTss[p].sample(Ct_patch*1.E-6)

      self.tc_patchTss[p].sample(tc_patch)
      self.td_patchTss[p].sample(td_patch)
      self.th_patchTss[p].sample(th_patch)
      self.tt_patchTss[p].sample(tt_patch)

      self.Dst_patchTss[p].sample(Dp)
      self.NPatch_patchTss[p].sample(NPatch)

      # per rumen fill
      self.tc_rumenTss[p].sample(tc_rumen/3600)
      self.td_rumenTss[p].sample(td_rumen/3600)
      self.th_rumenTss[p].sample(th_rumen/3600)
      self.tt_rumenTss[p].sample(tt_rumen/3600)

      # per Day
      self.Cc_dayTss[p].sample(Cc_day*1.E-6)
      self.Cd_dayTss[p].sample(Cd_day*1.E-6)
      self.Ch_dayTss[p].sample(Ch_day*1.E-6)
      self.Ct_dayTss[p].sample(Ct_day*1.E-6)

      self.tc_dayTss[p].sample(tc_day/3600)
      self.td_dayTss[p].sample(td_day/3600)
      self.th_dayTss[p].sample(th_day/3600)
      self.tt_dayTss[p].sample(tt_day/3600)

      self.tbout_dayTss[p].sample(t_bout/3600)
      self.Nbout_dayTss[p].sample(N_bout)

      # herbivore output
      self.RspTss[p].sample(Rsp)
      self.GrwTss[p].sample(Grw)
      self.dHTss[p].sample(self.dH[p])
      self.dH_maxTss[p].sample(self.dH_max[p])
      self.dH_max_relTss[p].sample(dH_max_rel)

      self.DnsHrbv_massTss[p].sample(self.DnsHrbv_b[p])
      self.DnsHrbv_numTss[p].sample(self.DnsHrbv_n[p]*1000)

      self.UtilTss[p].sample(self.Util[p])
      self.DnsH_low_star_massTss[p].sample(self.DnsH1_star_b[p])
      self.DnsH_high_star_massTss[p].sample(self.DnsH2_star_b[p])
      self.DnsH_low_star_numTss[p].sample(self.DnsH1_star_n[p]*1000)
      self.DnsH_high_star_numTss[p].sample(self.DnsH2_star_n[p]*1000)
      self.U_low_starTss[p].sample(self.U1_star[p])
      self.U_high_starTss[p].sample(self.U2_star[p])

model = ESpace("clone.cln")
dyn = DynamicFramework(model, 50)
dyn.run()

