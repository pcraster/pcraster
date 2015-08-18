## #!/usr/bin/env python

import math, sys, pcr, time, string, bifurcation, bifstats
from pcraster import *
import first, sys, pcr_python, dyn, mldd, block, glob, shutil

nrOfRows=sys.argv[1]
nrOfCols=sys.argv[2]

import inputs
nrOfTimeSteps=inputs.nrOfTimeSteps
nrOfSamples=inputs.nrOfSamples
printToScreen=inputs.printToScreen
saveExtraVariables=inputs.saveExtraVariables
reportInterval=inputs.reportInterval
reportIntervalQFractionStreams=inputs.reportIntervalQFractionStreams
seed=inputs.seed
diffusionParameter=inputs.diffusionParameter

setrandomseed(seed)

def execute(command):
  # alternatief voor os.system
  tokens = command.split()
  status = os.spawnvp(os.P_WAIT, tokens[0], tokens)

def cellValueMissingValueChecked(Map,Row,Column):
  Value,Valid=cellvalue(Map,Row,Column)
  if Valid:
    return Value
  else:
    print 'missing value in cellValueMissingValueChecked'

#def mapToList(Map,nrRows,nrCols):
#  row=0
#  nr=1
#  while row < nrRows:
#    row=row+1
#    col=0
#    while col < nrCols:
#      value,valid=cellvalue(Map,row,col)
#      if valid:
#        print nr, value
#        nr=nr+1
#      col=col+1

def totalProbability(Map):
  ID=uniqueid(ifthen(defined(Map),boolean(1)))
  MaxIDInt=cellValueMissingValueChecked(mapmaximum(ID),0,0)
  i=1
  Sum=1.0
  while i <= MaxIDInt:
    Value=mapmaximum(ifthen(pcreq(ID, scalar(i)),Map))
    ValueFloat=cellValueMissingValueChecked(Value,0,0)
    Sum=Sum*(1.0-ValueFloat)
    i = i+1
  totProbFloat=(1.0-Sum)
  if printToScreen:
    print 'total probability for bifurcation is ', str(totProbFloat)
  return ifthenelse(defined(Map),scalar(totProbFloat),scalar(totProbFloat))


class ModelData:

  def __init__(self):
    self.d_clone = "clone.map"
    x = self.d_clone
    # TIME BEING self.Y is distance to top of modelling area
    # this should stay like this but might change when
    # yt2b is killed in PCRaster..
    self.Y=scalar(0.0)-ycoordinate(x)
    YMax=ifthen(x,scalar(mapmaximum(self.Y)))
    YMin=ifthen(x,scalar(mapminimum(self.Y)))
    self.nrRows=((YMax-YMin)/celllength())+1
    self.nrRowsInteger=int(cellValueMissingValueChecked(self.nrRows,1,1))
    self.X=xcoordinate(x)
    XMax=ifthen(x,scalar(mapmaximum(self.X)))
    XMin=ifthen(x,scalar(mapminimum(self.X)))
    self.nrCols=((XMax-XMin)/celllength())+1
    self.nrColsInteger=int(cellValueMissingValueChecked(self.nrCols,1,1))
    self.Bot=pcreq(YMax,self.Y)
    self.BotTwoRows=pcror(self.Bot,pcreq(YMax-celllength(),self.Y))
    self.Top=pcreq(YMin,self.Y)
    self.Right=pcreq(XMax,self.X)
    self.Left=pcreq(XMin,self.X)
    self.MapEdges=pcror(pcror(self.Bot,self.Top),pcror( self.Right,self.Left))
    self.FloodplainLength=mapmaximum(self.Y)-mapminimum(self.Y)
    self.CF=ifthen(x,scalar(inputs.CF))                   # dep rate at infinite distance from belts, fraction of A
    self.BF=ifthen(x,scalar(inputs.BF))                   # parameter, B_F = Zmax/b, b ranges from 0.35-1.4, Zmax is
    self.WidthStream=ifthen(x,scalar(100.0))
    CentreCoordinate = (mapmaximum(self.X)-mapminimum(self.X))/2
    self.InflowPoint=pcrand(pcrand((pcrgt(CentreCoordinate+(celllength()/2),self.X)), \
                     pcrle((CentreCoordinate-(celllength()/2)),self.X)),self.Top)
    self.InflowPointX=cellValueMissingValueChecked(CentreCoordinate/celllength()-0.5,1,1)
    self.Duration=ifthen(self.d_clone,scalar(1.0))

  def clone(self):
    return self.d_clone


class SampleData:

 def __init__(self,clone, X, Y, InflowPoint, nrOfTimeSteps, Bot, Top, diffusionParameter):
   self.nrOfTimeSteps=nrOfTimeSteps
   self.diffusionParameter=diffusionParameter
   self.d_clone = clone
   self.TimeStepYears=ifthen(self.d_clone,scalar(1))
   self.TimeStepYearsFloat=1.0
   self.ActiveChannelsAgeYears=ifthen(self.d_clone,scalar(0))
   #self.UpstreamSlope=ifthen(self.d_clone,scalar(0.0001))
   self.UpstreamSlope=ifthen(self.d_clone,scalar(inputs.UpstreamSlope))
   self.Elevation=-1*self.UpstreamSlope*Y+uniform(1)/scalar(50)
   #self.Elevation=-1*self.UpstreamSlope*Y+windowaverage(uniform(1)/scalar(5),celllength()*5)
   #ElevFlood=ifthen(pcreq(SlopeExponent,0.0),-1*UpstreamSlope*Y,UpstreamSlope*(FloodplainLength/SlopeExponent) \
   #           *(exp((-1*SlopeExponent-0.000000001)*Y/FloodplainLength)-1))
   self.ElevationIni=self.Elevation
   self.InputChannelsCubePerYear=ifthen(self.d_clone,scalar(0.0))
   self.YMap=(mapmaximum(Y)+0.5*celllength())-Y
   self.SlightlySlopingSurface=self.YMap/10000000
   self.XMap=X
   self.Bot=Bot
   self.Top=Top
   # number of samples map for overbank distance calculation (inversedistance)
   distanceFromTopOrBot=spread(pcror(self.Bot,self.Top),scalar(0),scalar(1))
   range=scalar(6000)
   nrSamplesAtTopOrBot=scalar(5)
   nrSamplesAtCentre=scalar(50)
   self.nrSamples=nrSamplesAtTopOrBot+(nrSamplesAtCentre-nrSamplesAtTopOrBot)* \
             (scalar(1)-exp( scalar(0)-sqr(distanceFromTopOrBot/range)))

   #### Tectonism ####

   ### faulting
   self.XFaultCent=5000.0          # centre of fault 
   self.YFaultCent=5000.0          # centre of fault
   self.FaultDir=190.0             # direction of fault
   self.Rf=3600.0                  # fault radius (take care with 0,90 and 180!)
   self.FaultOn=boolean(1)         # faulting on (1) or off (0)
   self.movementFault()

   ## tilting
   self.XTiltCent=5000.0           # centre of tilt axis 
   self.YTiltCent=0.0              # centre of tilt axis
   self.TiltDir=0.1                # zero is axis from left to right
   self.Ddv=60000000.0              # distance from the tilt axis with subsidence is 1 unit, if positive, below
                                   # tilt axis uplift
   self.TiltOn=boolean(1)          # tilting on (1) or off (0)
   self.movementTilting()

   # sea level
   ## DO NOT CHANGE RiseDur or FallDur !!! (goes wrong)
   self.RiseDur= inputs.RiseDur       # duration of rise of Late-Q (yr)
   self.FallDur=inputs.FallDur        # duration of fall of Late-Q  (yr)
   self.RiseR=inputs.RiseR            # rise rate during the period of RiseDur (m/yr)
   self.FallR=inputs.FallR            # long term fall rate during the period of FallDur (m/yr)
   #self.Ampl=10.0000001              # amplitude of change (m), diff betweeen lowest and highest stand
   self.Ampl=inputs.Ampl              # amplitude of change (m), diff betweeen lowest and highest stand
   self.WaveL=inputs.WaveL            # short term wave length of rise (yr), duration between two high stands
   self.Start=inputs.Start            # start on sinus wave (degrees), 0 is first fall, 180 is first rise
   self.SeaLevelCurveFile='sl.tss'

   # sediment input at inflow point
   self.InitialSedimentInputCubePerYear=inputs.InitialSedimentInputCubePerYear
   self.RiseDurSediment= inputs.RiseDurSediment      # duration of rise of Late-Q (yr)
   self.FallDurSediment=inputs.FallDurSediment       # duration of fall of Late-Q  (yr)
   self.RiseRSediment=inputs.RiseRSediment           # rise rate during the period of RiseDur (m/yr)
   self.FallRSediment=inputs.FallRSediment           # long term fall rate during the period of FallDur (m/yr)
   self.AmplSediment=inputs.AmplSediment             # amplitude of change (m), diff betweeen lowest and highest stand
   self.WaveLSediment=inputs.WaveLSediment           # short term wave length of rise (yr), duration between two high stands
   self.StartSediment=inputs.StartSediment           # start on sinus wave (degrees), 0 is first level fall, 180 is first rise
   self.SedimentCurveFile='material.tss'

   # water input at inflow point
   self.InitialWaterInputCubePerSecond=inputs.InitialWaterInputCubePerSecond
   self.RiseDurWater= inputs.RiseDurWater    # duration of rise of Late-Q (yr)
   self.FallDurWater=inputs.FallDurWater      # duration of fall of Late-Q  (yr)
   self.RiseRWater=inputs.RiseRWater         # rise rate during the period of RiseDur (m/yr)
   self.FallRWater=inputs.FallRWater         # long term fall rate during the period of FallDur (m/yr)
   #self.AmplWater=1250.00000001             # amplitude of change (m), diff betweeen lowest and highest stand
   self.AmplWater=inputs.AmplWater           # amplitude of change (m), diff betweeen lowest and highest stand
   self.WaveLWater=inputs.WaveLWater         # short term wave length of rise (yr), duration between two high stands
   self.StartWater=inputs.StartWater         # start on sinus wave (degrees), 0 is first fall, 180 is first rise
   self.WaterCurveFile='water.tss'

   # block
   self.d_block=block.block()
   self.d_block.configure(1,1)
   self.d_block.setBase(self.ElevationIni)

 def generateCyclicTimeseries(self,InitialSeaLevel, RiseDur,FallDur,RiseR,FallR,Ampl,WaveL,Start,TimeStepYears,NrTimeSteps,timeSeries):
    SL=0.0
    FallTime=0.0
    RiseTime=0.0
    Fall=1
    Rise=0

    outputFile = open(timeSeries, 'w')
    Time=0.0

    for x in range(1, NrTimeSteps+1):
        RealTime=x*TimeStepYears
        Degrees=(Time/WaveL)*360.0+Start
        ## sea level fall (m/y)
        SLfall=FallR + 2.0 * 3.14 * 0.5*Ampl* math.sin(math.radians(Degrees))/WaveL
        SLrise=RiseR + 2.0 * 3.14 * 0.5*Ampl* math.sin(math.radians(Degrees))/WaveL

        ## duration of fall
        # pcrcalc: FallTime=mapifelse(Rise,0,FallTime+TimeStepYears)
        if Rise:
          FallTime = 0.0
        else:
          FallTime = FallTime+TimeStepYears

        Time=Time+TimeStepYears

        Fall=(FallTime < FallDur) and not Rise;

        ## duration of rise
        # pcrcalc: RiseTime=if(Fall,0,RiseTime+T);
        if Fall:
          RiseTime = 0.0
        else:
          RiseTime = RiseTime+TimeStepYears

        Rise=(RiseTime < RiseDur) and not Fall;

        ## convert sea level change rate to magnitude of elevation
        # pcrcalc SL= SL + if(Fall, -SLfall*TimeStepYears, SLrise*TimeStepYears);
        if Fall:
          SL = SL - SLfall*TimeStepYears
        else:
          SL = SL + SLrise*TimeStepYears

        SeaLevel=SL+InitialSeaLevel

        xFormat = '%7d' % (x)
        SeaLevelFormat = '%5.10f' % (SeaLevel)
        String = xFormat + ' ' +SeaLevelFormat + '\n'
        outputFile.write(String)

    outputFile.close()


 def generateAllTimeseries(self):
    # sea level
    self.generateCyclicTimeseries(cellValueMissingValueChecked(mapminimum(self.Elevation),0,0), \
                  self.RiseDur,self.FallDur, \
                  self.RiseR,self.FallR,self.Ampl,self.WaveL, \
                  self.Start,self.TimeStepYearsFloat,self.nrOfTimeSteps,
                  self.SeaLevelCurveFile)

    # sediment input
    self.generateCyclicTimeseries(self.InitialSedimentInputCubePerYear, \
                  self.RiseDurSediment,self.FallDurSediment, \
                  self.RiseRSediment,self.FallRSediment,self.AmplSediment,
                  self.WaveLSediment, \
                  self.StartSediment,self.TimeStepYearsFloat,self.nrOfTimeSteps,
                  self.SedimentCurveFile)

    # water input
    self.generateCyclicTimeseries(self.InitialWaterInputCubePerSecond, \
                  self.RiseDurWater,self.FallDurWater, \
                  self.RiseRWater,self.FallRWater,self.AmplWater,
                  self.WaveLWater, \
                  self.StartWater,self.TimeStepYearsFloat,self.nrOfTimeSteps,
                  self.WaterCurveFile)


 def movementFault(self):
   YFault=(0.0-(self.XMap-self.XFaultCent))/sin(self.FaultDir)+ \
          (self.YMap-self.YFaultCent)*cos(self.FaultDir)- \
          (0.0-(self.XMap-self.XFaultCent))*tan(90.0-self.FaultDir)*cos(self.FaultDir) 
   XFault=(self.XMap-self.XFaultCent)/cos(self.FaultDir)+ \
          (self.YMap-self.YFaultCent)*sin(self.FaultDir)- \
          (self.XMap-self.XFaultCent)*tan(self.FaultDir)*sin(self.FaultDir)
   Axes=pcror((pcrand(pcrle(XFault,celllength()/2.0), pcrgt(XFault,(0.0-celllength())/2.0))), \
        (pcrand(pcrle(YFault, celllength()/2.0) , pcrgt(YFault, (0.0-celllength())/2.0))))
   # fault displacement at faultplane
   rn=abs(XFault/self.Rf)
   dn=2.0*(1.0-rn)*sqrt(sqr(((1.0+rn)/2.0))-sqr(rn))
   Rd=self.Rf*sqrt(dn)
   rnd=min(1.0,abs(YFault)/Rd)
   dnd=cover(exp(0.0-5.5*rnd) - 0.004*rnd,0)
   self.TectMovFau=ifthenelse(self.FaultOn,ifthenelse(pcrgt(YFault, 0),dnd,0.0-dnd),0)

 def movementTilting(self):
   YTilt=(0.0-(self.XMap-self.XTiltCent))/sin(self.TiltDir)+ \
         (self.YMap-self.YTiltCent)*cos(self.TiltDir)-(0.0-(self.XMap-self.XTiltCent))* \
         tan(90.0-self.TiltDir)*cos(self.TiltDir)
   TiltAxis=pcrand(pcrle(YTilt, celllength()/2.0), \
            pcrgt(YTilt, 0.0-celllength()/2.0))
   self.TectMovTilt=ifthenelse(self.TiltOn,YTilt * (0.0-1.0 / self.Ddv),0.0)

 def updateActiveChannelsAgeYears(self, ChannelsOrdinal):
  self.ActiveChannelsAgeYears=ifthenelse(pcrne(ChannelsOrdinal,ordinal(0)), \
                              self.ActiveChannelsAgeYears+self.TimeStepYears,scalar(0))


 def erosion(self, ChannelsOrdinal, ChannelBelt, SeaLevel):
  # belts trench
  BooleanChannels=pcrne(ChannelsOrdinal,ordinal(0))
  ScalarZeroAtChannelBelt=ifthen(ChannelBelt,scalar(0.0))
  ScalarOneAtChannelBelt=ifthen(ChannelBelt,scalar(1.0))
  DistanceToChannels=spread(BooleanChannels,ScalarZeroAtChannelBelt,ScalarOneAtChannelBelt)
  DistanceToChannelsAtBelts=ifthen(ChannelBelt,DistanceToChannels)
  DTCABMax=mapmaximum(DistanceToChannelsAtBelts)
  self.DT=cover(scalar(0.2)-(DistanceToChannelsAtBelts-DTCABMax)/(DTCABMax+0.00001),scalar(0))
  # ldd
  Ldd=lddcreate(self.Elevation-self.DT,1e31,1e31,1e31,1e31)
  self.LddMod=lddrepair(ifthenelse(pcrne(ChannelsOrdinal,ordinal(0)),ldd(5),Ldd))
  Flux=accuflux(self.LddMod,inputs.P)
  Slope=pcr_python.slopedownstream(Ldd,self.Elevation)
  Slope=max(0.0000001,Slope)
  Tmp=scalar(inputs.gamma)*cover(ifthenelse(ChannelBelt,scalar(0), \
                              (Flux ** scalar(inputs.alpha))*(Slope ** scalar(inputs.beta))),scalar(0))
  self.Erosion=ifthenelse(pcrgt(Tmp,scalar(0.1)),scalar(0.1),Tmp)
  # input at channel cells, m height over whole channel cell
  InputChannelsMetrePerYear=cover(ifthen(pcrne(ChannelsOrdinal,ordinal(0)),accuflux(self.LddMod,self.Erosion)),scalar(0))
  self.InputChannelsCubePerYear=InputChannelsMetrePerYear*cellarea()
  self.Elevation=self.Elevation-self.Erosion
  # adjust DEM at bottom cells
  self.Elevation=ifthenelse(self.Bot,scalar(SeaLevel),self.Elevation)
  # adjust DEM when below sealevel
  # SlightlySlopingSurface is to prevent flat areas that cause problems with lddcreate (very
  # strange ldd's)
  # this has been tested and to prevent these quite flat areas 1.5 seems to be appropriate
  self.Elevation=max(scalar(SeaLevel-scalar(1.5))+self.SlightlySlopingSurface,self.Elevation)



 def depositionOverbankWindowOld(self, CF, BF, ChannelBelt, ChannelBeltEdge):
   # DJ area lower is still with spreadzone (resulting in edges)
   # maybe this needs to be with windowaverage, too!
   #
   # this function can be used to replace depositionOverbank
   # distance to edge of channel belt (m)
   EdgeBeltID=cover(nominal(uniqueid(ChannelBeltEdge)),nominal(0))
   # spreadzones
   self.BZone=ifthenelse(pcrnot(ChannelBelt),spreadzone(EdgeBeltID,0.0,1.0),EdgeBeltID)
   DistanceToBelts=spread(ChannelBelt,0.0,1.0)
   BCh=ifthenelse(ChannelBeltEdge,ifthenelse(pcrlt(self.DepositionBelts,0.0),0.0,self.DepositionBelts),-100.0)
   ## for averaging the deposition at the channel belt edge
   WindowLength=DistanceToBelts*2*1.2 
   BChZ=windowaverage(BCh,WindowLength)
   # elevation at edge of belt 
   BH=cover(ifthenelse(ChannelBeltEdge, self.Elevation,-1e31) ,scalar(-1e31))
   # area lower than nearest part of channel belt  !!!
   Tmp=areamaximum(BH,self.BZone)
   self.Lower=pcrge(Tmp+0.5,self.Elevation)
   #DepositionOverbankEveryWhere=windowaverage(ifthenelse(self.Lower, \ 
   #                              BChZ*(CF+(1.0-CF)*exp(-1*DistanceToBelts/BF)),0.0),3.0*celllength())
   DepositionOverbankEveryWhere=ifthenelse(self.Lower,BChZ*(CF+(1.0-CF)*exp(-1*DistanceToBelts/BF)),0.0)
    # deposition should be less than or eq to height of neirest channel belt minus height
    # and zero when height is greater than nearest channel belt height
    # this caused a problem with deposition around new channels and is switched off
    #BeltHeightMinusHeight=Tmp-self.Elevation
    #BeltHeightMinusHeight=ifthenelse(pcrlt(BeltHeightMinusHeight,scalar(0.0)),scalar(0.0),BeltHeightMinusHeight)
    #DepositionOverbankEveryWhere=mapifelse \
    #       (pcrgt(DepositionOverbankEveryWhere,BeltHeightMinusHeight),BeltHeightMinusHeight,DepositionOverbankEveryWhere)
   # TIME BEING
   DepositionOverbankEveryWhere=windowaverage(DepositionOverbankEveryWhere,DistanceToBelts/2)
   # END TIME BEING
   # no deposition overbank at channel belt
   self.DepositionOverbank=ifthen(pcrnot(ChannelBelt),DepositionOverbankEveryWhere)
   # TIME BEING FOR REPORT

 def depositionOverbankWindowNew(self, CF, BF, ChannelBelt, ChannelBeltEdge):
   EdgeBeltID=cover(nominal(uniqueid(ChannelBeltEdge)),nominal(0))
   # spreadzones
   self.BZone=ifthenelse(pcrnot(ChannelBelt),spreadzone(EdgeBeltID,0.0,1.0),EdgeBeltID)
   DistanceToBelts=spread(ChannelBelt,0.0,1.0)
   BCh=ifthenelse(ChannelBeltEdge,ifthenelse(pcrlt(self.DepositionBelts,0.0),0.0,self.DepositionBelts),-100.0)
   self.BChZ=inversedistance(self.d_clone,BCh,scalar(2.0), scalar(0),self.nrSamples)
   # elevation at edge of belt 
   BH=cover(ifthenelse(ChannelBeltEdge, self.Elevation,-1e31) ,scalar(-1e31))
   # area lower than nearest part of channel belt  !!!
   Tmp=areamaximum(BH,self.BZone)
   self.Lower=pcrge(Tmp+0.5,self.Elevation)
   DepositionOverbankEveryWhere=ifthenelse(self.Lower,self.BChZ*(CF+(1.0-CF)*exp(-1*DistanceToBelts/BF)),0.0)
    # deposition should be less than or eq to height of neirest channel belt minus height
    # and zero when height is greater than nearest channel belt height
    # this caused a problem with deposition around new channels and is switched off
    #BeltHeightMinusHeight=Tmp-self.Elevation
    #BeltHeightMinusHeight=ifthenelse(pcrlt(BeltHeightMinusHeight,scalar(0.0)),scalar(0.0),BeltHeightMinusHeight)
    #DepositionOverbankEveryWhere=mapifelse \
    #       (pcrgt(DepositionOverbankEveryWhere,BeltHeightMinusHeight),BeltHeightMinusHeight,DepositionOverbankEveryWhere)
   # no deposition overbank at channel belt
   self.DepositionOverbank=ifthen(pcrnot(ChannelBelt),DepositionOverbankEveryWhere)

class Digraph:

 def __init__(self, LddN, LddNE, LddE, LddSE, LddS, LddSW, LddW, LddNW, InflowPoint, Bot, Top, Duration, MapEdges):
   ArcsWithPits = [LddN,LddNE,LddE,LddSE,LddS,LddSW,LddW,LddNW]
   self.Arcs=[]
   for arc in ArcsWithPits:
    self.Arcs.append(ifthen(pcror((pcrge(upstream(arc,scalar(1)),scalar(1))),(pcrne(arc,5))),arc))

   self.Channel=pcror(self.startNodeOccurs(),self.endNodeOccurs())
   self.d_mldd=mldd.mldd()
   MVLddMap=ifthen(pcrnot(boolean(LddN)),ldd(1))
   self.d_mldd.setStream(MVLddMap, MVLddMap, MVLddMap, MVLddMap, MVLddMap, MVLddMap, MVLddMap, MVLddMap)
   self.ChannelsOrdinal=ifthenelse(InflowPoint,ordinal(0),ordinal(0))

   self.creationYear=[]
   for i in range(len(self.Arcs)):
    self.creationYear.append(ifthen(pcrne(self.Arcs[i],ldd(5)),scalar(1e31)))

   self.InflowPoint=InflowPoint
   self.Bot=Bot
   self.Top=Top
   self.WindowThresholdForChannelBeltEdgeOnChannelBelt=ifthenelse(pcror(self.Bot,self.Top),scalar(6),scalar(9)) 
   self.Clone=ifthenelse(Bot,boolean(1),boolean(1))

#   self.Slope=[]
#   for i in range(len(self.Arcs)):
#    self.Slope.append(ifthen(pcrnot(self.Clone),scalar(0)))

   self.manningsN=scalar(0.04)
   self.z=scalar(1.0)
   self.WidthChannelBelt=self.scalar(scalar(-1e31))
   self.Duration=Duration
   self.MapEdges=MapEdges
   self.bankErodibility=scalar(inputs.bankErodibility)
   self.BifurcationAndChannelID=ordinal(0)
   self.AllBifurcationLocs=ordinal(0)
   self.AvulsionLocationOnChannel=InflowPoint
   self.ChannelsOrdinal=ifthenelse(InflowPoint,ordinal(1),ordinal(0))

 def endNode(self,Map):
  endNodeList=[]
  for arc in self.Arcs:
   endNodeList.append(ifthen(pcrne(arc,ldd(5)),downstream(arc,Map)))
  return endNodeList

 def startNode(self):
  startNodeList=[]
  for arc in self.Arcs:
   startNodeList.append(pcrne(arc,ldd(5)))
  return startNodeList

 def arcLength(self):
  arcLengthList=[]
  for arc in self.Arcs:
   arcLengthList.append(downstreamdist(arc))
  return arcLengthList

 def divide(self,DigraphScalarOne,ScalarMap):
  arcDivideList=[]
  for i in range(len(self.Arcs)):
   arcDivideList.append(DigraphScalarOne[i]/ScalarMap)
  return arcDivideList

 def plus(self,DigraphScalarOne,ScalarMap):
  arcDivideList=[]
  for i in range(len(self.Arcs)):
   arcDivideList.append(DigraphScalarOne[i]+ScalarMap)
  return arcDivideList

 def gt(self,DigraphScalarOne,ScalarMap):
  arcDivideList=[]
  for i in range(len(self.Arcs)):
   arcDivideList.append(pcrgt(DigraphScalarOne[i],ScalarMap))
  return arcDivideList

 def eq(self,Digraph,Map):
  arcDivideList=[]
  for i in range(len(self.Arcs)):
   arcDivideList.append(pcreq(Digraph[i],Map))
  return arcDivideList

 def andje(self,Digraph,Map):
  arcDivideList=[]
  for i in range(len(self.Arcs)):
   arcDivideList.append(pcrand(Digraph[i],Map))
  return arcDivideList

 def lt(self,DigraphScalarOne,ScalarMap):
  arcDivideList=[]
  for i in range(len(self.Arcs)):
   arcDivideList.append(pcrlt(DigraphScalarOne[i],ScalarMap))
  return arcDivideList

 def min(self,DigraphScalarOne,ScalarMap):
  arcDivideList=[]
  for i in range(len(self.Arcs)):
   arcDivideList.append(min(DigraphScalarOne[i],ScalarMap))
  return arcDivideList

 def max(self,DigraphScalarOne,ScalarMap):
  arcDivideList=[]
  for i in range(len(self.Arcs)):
   arcDivideList.append(max(DigraphScalarOne[i],ScalarMap))
  return arcDivideList

 def digraphDivide(self,DigraphScalarOne,DigraphScalarTwo):
  arcList=[]
  for i in range(len(self.Arcs)):
   arcList.append(DigraphScalarOne[i]/DigraphScalarTwo[i])
  return arcList

 def digraphOr(self,DigraphBooleanOne,DigraphBooleanTwo):
  arcList=[]
  for i in range(len(self.Arcs)):
   arcList.append(pcror(DigraphBooleanOne[i],DigraphBooleanTwo[i]))
  return arcList

 def digraphMinus(self,DigraphScalarOne,DigraphScalarTwo):
  arcList=[]
  for i in range(len(self.Arcs)):
   arcList.append(DigraphScalarOne[i]-DigraphScalarTwo[i])
  return arcList

 def digraphPlus(self,DigraphScalarOne,DigraphScalarTwo):
  arcList=[]
  for i in range(len(self.Arcs)):
   arcList.append(DigraphScalarOne[i]+DigraphScalarTwo[i])
  return arcList

 def digraphCover(self,DigraphScalarOne,DigraphScalarTwo):
  arcList=[]
  for i in range(len(self.Arcs)):
   arcList.append(cover(DigraphScalarOne[i],DigraphScalarTwo[i]))
  return arcList

 def cover(self,DigraphScalar,ScalarMap):
  List=[]
  for i in range(len(self.Arcs)):
   List.append(cover(DigraphScalar[i],ScalarMap))
  return List

 def diMax(self,DigraphScalarOne,ScalarMap):
  maxList=[]
  for i in range(len(self.Arcs)):
   maxList.append(ifthenelse(pcrlt(DigraphScalarOne[i],ScalarMap),ScalarMap,DigraphScalarOne[i]))
  return maxList

 def digraphMax(self,DigraphScalarOne,DigraphScalarTwo):
  maxList=[]
  for i in range(len(self.Arcs)):
   maxList.append(ifthenelse(pcrlt(DigraphScalarOne[i],DigraphScalarTwo[i]),DigraphScalarTwo[i],DigraphScalarOne[i]))
  return maxList

 def multiply(self,DigraphScalarOne,ScalarMap):
  arcDivideList=[]
  for i in range(len(self.Arcs)):
   arcDivideList.append(DigraphScalarOne[i]*ScalarMap)
  return arcDivideList

 def digraphMultiply(self,DigraphScalarOne,DigraphScalarTwo):
  arcMultiplyList=[]
  for i in range(len(self.Arcs)):
   arcMultiplyList.append(DigraphScalarOne[i]*DigraphScalarTwo[i])
  return arcMultiplyList

 def pow(self,DigraphScalar,ScalarMap):
  List=[]
  for i in range(len(self.Arcs)):
   List.append(pow(DigraphScalar[i],ScalarMap))
  return List

 def scalar(self,ScalarMap):
  scalarList=[]
  for arc in self.Arcs:
   scalarList.append(ifthen(pcrne(arc,ldd(5)),ScalarMap))
  return scalarList

 def slope(self,ScalarMap):
  arcSlopeList=[]
  EndNodeDem=self.endNode(ScalarMap)
  ArcLength=self.arcLength()
  arcSlopeList=[]
  for i in range(len(self.Arcs)):
    arcSlopeList.append((ScalarMap-EndNodeDem[i])/ArcLength[i])
  return arcSlopeList

 def sqrt(self,DigraphScalar):
  sqrtList=[]
  for i in range(len(self.Arcs)):
   sqrtList.append(sqrt(DigraphScalar[i]))
  return sqrtList

 def conversionScalar(self,Digraph):
  sqrtList=[]
  for i in range(len(self.Arcs)):
   sqrtList.append(scalar(Digraph[i]))
  return sqrtList

 def uniform(self):
  arcUniformList=[]
  for arc in self.Arcs:
   arcUniformList.append(uniform(pcrne(arc,5)))
  return arcUniformList

 def normal(self):
  arcUniformList=[]
  for arc in self.Arcs:
   arcUniformList.append(normal(pcrne(arc,5)))
  return arcUniformList

 def upstream(self,DigraphScalar):
  upstreamSum=scalar(0.0)
  for i in range(len(self.Arcs)):
   upstreamSum=upstreamSum+cover(upstream(self.Arcs[i],cover(DigraphScalar[i],scalar(0))),scalar(0))
  return ifthen(self.Channel,upstreamSum)

 def sumOfEdges(self, DigraphScalar):
  arcSum=scalar(0.0)
  for i in range(len(self.Arcs)):
   arcSum=arcSum+cover(DigraphScalar[i],scalar(0))
  Channels=pcror(self.startNodeOccurs(),self.endNodeOccurs())
  return ifthen(Channels,arcSum)

 def minOfEdges(self, DigraphScalar):
  arcMin=scalar(1e31)
  for i in range(len(self.Arcs)):
   tmp=cover(DigraphScalar[i],scalar(1e31))
   arcMin=ifthenelse(pcrlt(arcMin,tmp),arcMin,tmp)
  Channels=pcror(self.startNodeOccurs(),self.endNodeOccurs())
  return ifthen(pcrand(pcrnot(self.Bot),Channels),arcMin)

 def maxOfEdges(self, DigraphScalar):
  arcMax=scalar(-1e31)
  for i in range(len(self.Arcs)):
   tmp=cover(DigraphScalar[i],scalar(-1e31))
   arcMax=ifthenelse(pcrgt(arcMax,tmp),arcMax,tmp)
  Channels=pcror(self.startNodeOccurs(),self.endNodeOccurs())
  return ifthen(pcrand(pcrnot(self.Bot),Channels),arcMax)

 def orOfEdges(self, DigraphBoolean):
  arcOr=boolean(0)
  for i in range(len(self.Arcs)):
   arcOr=pcror(arcOr,DigraphBoolean[i])
  return arcOr

 def startNodeOccurs(self):
  startNode=boolean(0)
  for arc in self.Arcs:
   startNode=pcror(startNode,cover(pcrne(arc,ldd(5)),boolean(0)))
  return startNode

 def endNodeOccurs(self):
  endNode=boolean(0)
  for arc in self.Arcs:
   endNode=pcror(endNode,boolean(cover(upstream(arc,scalar(1)),scalar(0.0))))
  return endNode

 def takeFirstEdgeOfBooleanTrues(self,DigraphBoolean):
  AnotherOneIsTrue=boolean(0)
  List=[]
  for i in range(len(self.Arcs)):
   List.append(ifthenelse(AnotherOneIsTrue,boolean(0),DigraphBoolean[i]))
   AnotherOneIsTrue=pcror(DigraphBoolean[i],AnotherOneIsTrue)
  return List

 def diffusion(self,Dem,Input,FixedHead,DiffusionValue,iterations):
  InputPerIteration=Input/scalar(iterations)
  DiffusionValuePerIteration=self.divide(DiffusionValue,scalar(iterations))
  for i in range(0,iterations):
   Dem=Dem+InputPerIteration
   Slope = self.slope(Dem)
   SlopeNoNegativeValues=self.diMax(Slope,scalar(0.0))
   Outflow = self.digraphMultiply(SlopeNoNegativeValues,DiffusionValuePerIteration)
   Inflow=self.upstream(Outflow)
   TotalOutflow=self.sumOfEdges(Outflow)
   Dem=ifthen(self.Channel,ifthenelse(FixedHead,Dem,Dem+Inflow-TotalOutflow))
  return Dem, TotalOutflow*scalar(iterations)

 def getStream(self):
  self.Arcs[0], self.Arcs[1], self.Arcs[2], self.Arcs[3], self.Arcs[4], self.Arcs[5], \
  self.Arcs[6], self.Arcs[7]=self.d_mldd.getStream()
  for i in range(len(self.Arcs)):
   self.Arcs[i] = cover(self.Arcs[i],ldd(5))

 def setChannel(self):
  self.Channel=pcror(self.startNodeOccurs(),self.endNodeOccurs())

 def setCreationYearStreams(self, currentYear):
  for i in range(len(self.Arcs)):
   self.creationYear[i]=ifthen(pcrne(self.Arcs[i],ldd(5)), cover(self.creationYear[i],scalar(currentYear)))

 def updateChannelsOrdinalAfterRemovingChannels(self):
  self.ChannelsOrdinal=ifthenelse(self.Channel,self.ChannelsOrdinal,ordinal(0))

 def setSlopeStreamsDeterministic(self, Dem):
  self.SlopeDeterministic=self.slope(Dem)
  self.SlopeGreaterThanZeroDeterministic=self.diMax(self.SlopeDeterministic,scalar(0.00001))

 def setSlopeStreamsRandom(self):
  #self.SlopeRandom=self.slope(Dem+normal(1)/200.0)
  #normalSD=self.multiply(self.normal(),scalar(0.05))
  normalSD=self.multiply(self.normal(),scalar(inputs.slopeBifSD))
  normalMeanOneSD=self.plus(normalSD,scalar(1.0))
  normalMeanOneSDCutoff=self.max(self.min(normalMeanOneSD,scalar(1.5)),scalar(0.5))
  self.SlopeRandom=self.digraphMultiply(self.SlopeDeterministic,normalMeanOneSDCutoff)
  self.SlopeGreaterThanZeroRandom=self.diMax(self.SlopeRandom,scalar(0.00001))

 def setQStreams(self, Input):
  # input is in m3/s
  PitAtBifurcationsLdd,Bifurcations=dyn.pitAtBifurcationsLdd(
           self.Arcs[0],self.Arcs[1],self.Arcs[2],self.Arcs[3],self.Arcs[4],self.Arcs[5],self.Arcs[6],self.Arcs[7])
  nrBifurcations=maptotal(ifthenelse(Bifurcations,scalar(1),scalar(0)))
  nrBifurcationsInteger=int(cellValueMissingValueChecked(nrBifurcations,0,0))

  SlopeNoNegativeValues=self.diMax(self.SlopeDeterministic,scalar(0.0))
  self.SqrtSlope=self.diMax(self.sqrt(SlopeNoNegativeValues),scalar(0.0000000001))
  YFlux, NTotFlux, NETotFlux, ETotFlux, SETotFlux, STotFlux, SWTotFlux, WTotFlux, NWTotFlux = \
  pcr_python.dispfluxperdirOpti( \
           Input, \
           cover(self.SqrtSlope[0],scalar(0.0)), \
           cover(self.SqrtSlope[1],scalar(0.0)), \
           cover(self.SqrtSlope[2],scalar(0.0)), \
           cover(self.SqrtSlope[3],scalar(0.0)), \
           cover(self.SqrtSlope[4],scalar(0.0)), \
           cover(self.SqrtSlope[5],scalar(0.0)), \
           cover(self.SqrtSlope[6],scalar(0.0)), \
           cover(self.SqrtSlope[7],scalar(0.0)), \
           self.Arcs[0],self.Arcs[1],self.Arcs[2],self.Arcs[3],self.Arcs[4],self.Arcs[5],self.Arcs[6],self.Arcs[7], \
           PitAtBifurcationsLdd, \
           nrBifurcationsInteger)
  self.QStreams=[]
  self.QStreams.append(NTotFlux)
  self.QStreams.append(NETotFlux)
  self.QStreams.append(ETotFlux)
  self.QStreams.append(SETotFlux)
  self.QStreams.append(STotFlux)
  self.QStreams.append(SWTotFlux)
  self.QStreams.append(WTotFlux)
  self.QStreams.append(NWTotFlux)
  for i in range(len(self.Arcs)):
   self.QStreams[i]=ifthen(pcrne(self.Arcs[i],ldd(5)),self.QStreams[i]+1)

 def setDiffusionStreams(self,DiffusionParameter):
  # TIME BEING, dimax, since very low values gives problems in diffusion equation
  self.DiffusionStreams=self.multiply(self.diMax(self.QStreams,scalar(100)),DiffusionParameter)
  #self.DiffusionStreams=self.multiply(self.QStreams,DiffusionParameter)
  DiffusionStreamsMap=self.maxOfEdges(self.DiffusionStreams)

 #def setDiffusionStreamsMovingAverageInitial(self):
 # self.DiffusionStreamsMovingAverage=self.multiply(self.DiffusionStreams,scalar(1.0))
 #
 #def setDiffusionStreamsMovingAverage(self):
 # self.DiffusionStreamsPrevious=self.multiply(self.DiffusionStreamsMovingAverage,scalar(1.0))
 # self.DiffusionStreamsMovingAverage=self.divide(self.digraphPlus(self.DiffusionStreamsPrevious, \
 #                                    self.DiffusionStreams),scalar(2.0))

 def setWidthChannel(self):
  QTerm=self.pow(self.QStreams,scalar(0.462))
  SlopeTerm=self.pow(self.SlopeGreaterThanZeroDeterministic,scalar(0.231))
  Division=self.digraphDivide(QTerm,SlopeTerm)
  self.WidthChannel=self.multiply(Division,scalar(0.5)*self.z)

 def setDepthChannel(self):
  QTerm=self.pow(self.QStreams,scalar(0.323))
  SlopeTerm=self.pow(self.SlopeGreaterThanZeroDeterministic,scalar(0.161))
  Division=self.digraphDivide(QTerm,SlopeTerm)
  self.DepthChannel=self.multiply(Division,scalar(1.52)*pow((self.manningsN/self.z),scalar(0.6)))

 def setVelocityChannels(self):
  QTerm=self.pow(self.QStreams,scalar(0.215))
  SlopeTerm=self.pow(self.SlopeGreaterThanZeroDeterministic,scalar(0.393))
  Product=self.digraphMultiply(QTerm,SlopeTerm)
  self.VelocityChannels=self.multiply( Product, scalar(1.32) / \
                        ( pow(self.manningsN,scalar(0.6)) * pow(self.z,scalar(0.4)) )  )

 def setMaxWidthChannelBelt(self):
  DepthTerm=self.pow(self.DepthChannel,scalar(1.8))
  self.MaxWidthChannelBelt=self.multiply(DepthTerm,scalar(59.86))

 def setWidthChannelBelt(self):
  UpdatedForChannelWidth=self.digraphMax(self.WidthChannelBelt,self.WidthChannel)
  UpdatedForNewChannels=self.digraphCover(UpdatedForChannelWidth,self.WidthChannel)
  DiffTerm=self.digraphMinus(self.MaxWidthChannelBelt,UpdatedForNewChannels)
  DiffTerm=self.diMax(DiffTerm,scalar(0.0))
  GrowthRatePerYear=self.multiply(DiffTerm,self.bankErodibility)
  Growth=self.multiply(GrowthRatePerYear,self.Duration)
  self.WidthChannelBelt=self.digraphPlus(UpdatedForNewChannels,Growth)
  self.WidthChannelBeltMap=self.maxOfEdges(self.WidthChannelBelt)
 
 def setMaxSlopeDeterministicStreams(self):
  self.MaxSlopeDeterministicStreamsMap=self.maxOfEdges(self.SlopeDeterministic)

 def calculationsForSafePropertiesStreams(self):
  self.QStr=self.maxOfEdges(self.QStreams)
  self.WidthChannelMap=self.maxOfEdges(self.WidthChannel)
  self.VelocityChannelsMap=self.maxOfEdges(self.VelocityChannels)
  self.MaxWidthChannelBeltMap=self.maxOfEdges(self.MaxWidthChannelBelt)
  self.TotalProbabilityForBifurcation=totalProbability(self.ProbBifurcationSlopeRatio)

 def createChannelBelt(self):
  DistanceFromChannelBeltEdge=spread(defined(self.WidthChannelBeltMap),scalar(0.0)-(self.WidthChannelBeltMap/2),scalar(1))
  self.ChannelBelt=pcrle(DistanceFromChannelBeltEdge,scalar(0))

 def depthAtChannelBeltAndCOAtChannelBelt(self):
   ChannelMV=ifthen(pcrand(self.Channel,pcrnot(self.Bot)),boolean(1))
   CentreID=nominal(cover(uniqueid(ChannelMV),scalar(0)))
   #SpreadZones=spreadzone(CentreID,scalar(0),ifthen(self.ChannelBelt,scalar(1)))
   # TIME BEING, this /4 is from testing, boundaries between channel belts is still a problem ..
   SpreadZones=spreadzone(CentreID,scalar(0.0)-(self.WidthChannelBeltMap/4),ifthen(self.ChannelBelt,scalar(1)))
   self.DepthChannelMap=self.maxOfEdges(self.DepthChannel)
   self.DepthAtChannelBelt=ifthen(self.ChannelBelt,
                              max( areamaximum(cover(self.DepthChannelMap,scalar(-10)),SpreadZones),
                              scalar(0.000001))
                              )
   self.ChannelBeltOrdinal=cover(ifthen(self.ChannelBelt,
                              ordinal(areamaximum(cover(scalar(self.ChannelsOrdinal),scalar(-10)),SpreadZones))),
                              ordinal(0))

 def createChannelBeltEdgeNormalAndOnChannelBelt(self):
  WindowTotal=windowtotal(ifthen(self.ChannelBelt,scalar(1)),celllength()*scalar(3))

  TmpA=ifthen(pcrnot(self.ChannelBelt),pcrlt(WindowTotal,scalar(9)))
  Tmp=pcrand(TmpA,pcrnot(self.MapEdges))
  self.ChannelBeltEdge=ifthen(Tmp,boolean(1))

  Tmp=ifthen(self.ChannelBelt,pcrlt(WindowTotal,self.WindowThresholdForChannelBeltEdgeOnChannelBelt))
  self.ChannelBeltEdgeOnChannelBelt=ifthen(Tmp,boolean(1))

 #def createLddTowardsChannelBelt(self):
 # print 'DIT KAN OOK MET DISTANCEFROMCHANNELBELTEDGE!! of met spread(-dem, andersom dus'
 # DistanceToChannel=spread(self.Channel,scalar(0),scalar(1))
 # Tmp=lddcreate(DistanceToChannel,1e31,1e31,1e31,1e31)
 # self.LddTowardsChannelBelt=ifthenelse(self.Channel,ldd(5),Tmp)

 def setRemoveStreamsLocations(self):
  self.TotalQStreams=self.sumOfEdges(self.QStreams)
  self.QFractionStreams=self.divide(self.QStreams,self.TotalQStreams)
  self.RemoveStreamDownstreamMV=self.lt(self.QFractionStreams,scalar(inputs.UCrit))
  self.MultipleStreamsProblem=self.cover(self.RemoveStreamDownstreamMV,boolean(0))
  self.RemoveStreamDownstream=self.takeFirstEdgeOfBooleanTrues(self.MultipleStreamsProblem)
  self.RemoveStreamDownstreamMap=self.orOfEdges(self.RemoveStreamDownstream)
  self.RemoveStreamsOccursPythonInteger = cellValueMissingValueChecked \
                                          (boolean(mapmaximum(scalar(self.RemoveStreamDownstreamMap))),0,0)

 def setRemoveStreamsLocationsWithSqrtSlope(self):
  self.SqrtSlopeRandom=self.diMax(self.sqrt(self.SlopeGreaterThanZeroRandom),scalar(0.0000000001))
  self.TotalSqrtSlopeRandom=self.sumOfEdges(self.SqrtSlopeRandom)
  self.SqrtSlopeFractionStreams=self.divide(self.SqrtSlopeRandom,self.TotalSqrtSlopeRandom)
  self.RemoveStreamDownstreamMV=self.lt(self.SqrtSlopeFractionStreams,scalar(inputs.UCrit))
  self.MultipleStreamsProblem=self.cover(self.RemoveStreamDownstreamMV,boolean(0))
  self.RemoveStreamDownstream=self.takeFirstEdgeOfBooleanTrues(self.MultipleStreamsProblem)
  self.RemoveStreamDownstreamMap=self.orOfEdges(self.RemoveStreamDownstream)
  self.RemoveStreamsOccursPythonInteger = cellValueMissingValueChecked \
                                          (boolean(mapmaximum(scalar(self.RemoveStreamDownstreamMap))),0,0)
# def setRemoveStreamsLocationsNew(self):
#  self.TotalQStreams=self.sumOfEdges(self.QStreams)
#  self.QFractionStreams=self.divide(self.QStreams,self.TotalQStreams)
#  self.RemoveStreamDownstreamMV=self.lt(self.QFractionStreams,scalar(inputs.UCrit))
#  self.MultipleStreamsProblem=self.cover(self.RemoveStreamDownstreamMV,boolean(0))
#  # calculate random remove locations
#  randomRemoveDownStream=self.setRemoveStreamsLocationsRandom()
#  # 'sum' all remove locations
#  self.AllLocations=self.digraphOr(self.MultipleStreamsProblem,randomRemoveDownStream)
#  self.RemoveStreamDownstream=self.takeFirstEdgeOfBooleanTrues(self.MultipleStreamsProblem)
#  self.RemoveStreamDownstreamMap=self.orOfEdges(self.RemoveStreamDownstream)
#  report(self.RemoveStreamDownstreamMap,"testje")
#  self.RemoveStreamsOccursPythonInteger = cellValueMissingValueChecked \
#                                          (boolean(mapmaximum(scalar(self.RemoveStreamDownstreamMap))),1,1)
#  print('jaaaaaa')
#  print(self.RemoveStreamsOccursPythonInteger)
#
# def setRemoveStreamsLocationsRandom(self):
#   AllActiveBifurcationLocsBoolean=cover(
#                                 pcrgt(self.sumOfEdges(self.conversionScalar(self.startNode())),scalar(1.0)),
#                                 boolean(0))
#   removeStreamDownStreamMap=pcrlt(uniform(boolean(1)),scalar(0.50))
#   removeStreamDownstreamMV=self.andje(self.startNode(),removeStreamDownStreamMap)
#   removeStreamDownstream=self.cover(removeStreamDownstreamMV,boolean(0))
#   return removeStreamDownstream

 def setPropertiesChannels(self, elevation, inflowPoint, waterInputCubePerDay,diffusionParameter,runChannelsGeometry,
                           tryToRemoveStreams):
   if printToScreen:
     print 'setPropertiesChannels is done'
   if runChannelsGeometry:
     self.setPropertiesChannelsGeometry(elevation, inflowPoint, waterInputCubePerDay, diffusionParameter)
     if printToScreen:
       print ' runChannelsGeometry is done'
   if tryToRemoveStreams:
     self.setPropertiesChannelsRemoveStreamsLocations()
     if printToScreen:
       print ' tryToRemoveStreams is done'

 def setPropertiesChannelsRemoveStreamsLocations(self):
   # note that this uses a lot from runChannelsGeometry, so when the mldd changes, runChannelsGeometry
   # should always be true
   self.setSlopeStreamsRandom()
   #self.setRemoveStreamsLocations()
   self.setRemoveStreamsLocationsWithSqrtSlope()

 def setPropertiesChannelsGeometry(self, elevation, inflowPoint,waterInputCubePerDay, diffusionParameter):
   self.setChannel()
   self.setSlopeStreamsDeterministic(elevation)
   self.setMaxSlopeDeterministicStreams()
   self.setQStreams(waterInputCubePerDay*scalar(inflowPoint))
   self.setDiffusionStreams(scalar(diffusionParameter))
   self.setWidthChannel()
   self.setDepthChannel()
   self.setVelocityChannels()
   self.setMaxWidthChannelBelt()
   self.setWidthChannelBelt()
   self.createChannelBelt()
   self.depthAtChannelBeltAndCOAtChannelBelt()
   self.createChannelBeltEdgeNormalAndOnChannelBelt()

 #def saveEdges(self, firstPartFileName, Edges):
 # extension = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']
 # for i in range(len(Edges)):
 #  report(Edges[i],  self.generateName(firstPartFileName + extension[i]))
 #
 #def savePropertiesChannels(self):
 # report(self.Channel, self.generateName("C"))
 # self.saveEdges("Slop", selfSlope)
 # self.saveEdges("QStr", self.QStreams)
 # self.saveEdges("DiStr", self.DiffusionStreams)
 # self.saveEdges("WStr", self.WidthChannel)
 # self.saveEdges("DStr", self.DepthChannel)
 # self.saveEdges("VStr", self.VelocityChannels)
 # self.saveEdges("MWCB", self.MaxWidthChannelBelt)
 # self.saveEdges("WCB", self.WidthChannelBelt)
 # report(self.WidthChannelBeltMap, self.generateName("WCBM"))
 # report(self.ChannelBelt, self.generateName("Beltje"))
 # report(self.ChannelBeltEdge, self.generateName("CBE"))
 # report(self.ChannelBeltEdgeOnChannelBelt, self.generateName("CBEOCB"))

 def avulsionLocation(self,Elevation, Y):
    ## note that this function uses the width of the channel belt at the avulsion
    ## location which is a bit counter intuitive
    ##Uniform=uniform(self.ChannelBeltEdge)
    ## boolean map with TRUE at avulsion location on channel belt edge, FALSE
    ## on channel belt edge and MV elsewhere
    #ALoc=pcreq(ifthen(self.Clone,mapmaximum(Uniform)),Uniform)
    # on channel belt distance to avulsion location at channel belt edge, MV elsewhere
    DistALoc=spread(cover(self.AvulsionLocation,ifthen(self.ChannelBelt,boolean(0))),scalar(0),scalar(1))
    # on channel distance to avulsion location at channel belt edge, MV elsewhere
    DistALocCentres=ifthen(self.Channel,DistALoc)
    # TRUE: cells on channel nearest to avulsion location on channel belt edge, FALSE:
    # rest of channel, MV elsewhere
    StartNewChs=pcreq(DistALocCentres,ifthen(self.Clone,mapminimum(DistALocCentres)))
    # in case of multiple cells with true on StartNewChs, take the upper one
    YStartNewChs=ifthen(StartNewChs,Y)
    # TRUE: avulsion location on channel, FALSE rest of map
    self.AvulsionLocationOnChannel=cover(pcreq(ifthen(self.Clone,mapminimum(YStartNewChs)),YStartNewChs),boolean(0))
    # make valley to connect between avulsion loc and StartnewCh as small as possible
    # else the cb gets to high far away from an self.AvulsionLocation
    # first assign width of channel at avulsion location on channel to all cells on map
    WidthChannelOnAvulsionLocationOnChannel=ifthen(self.AvulsionLocationOnChannel,DistALoc)
    Tmp=cover(WidthChannelOnAvulsionLocationOnChannel,scalar(-1e31))
    WidthChannelEveryWhere=ifthenelse(defined(Y),mapmaximum(Tmp),mapmaximum(Tmp))+(scalar(3.0)*celllength())+scalar(0.1)
    # then calculate the total dem, i.e. on the channel belt you add zero at the avulsion
    # location on the channel belt edge and an uphill zone in all directions (including the
    # avulsion location at the channel)
    #DemLdd= Elevation+cover(ifthenelse(pcrgt(DistALoc,0.5*WidthChannelEveryWhere),scalar(0),DistALoc),scalar(0))
    DemLdd= Elevation+cover(ifthenelse(pcrgt(DistALoc,WidthChannelEveryWhere),scalar(0),DistALoc),scalar(0))
    # OLD DemLdd= Elevation+cover(ifthenelse(pcrgt(DistALoc,0.75*MaxWidthBeltForAvulsionLocation),  \
    #             scalar(0),DistALoc),scalar(0))
    AreaAroundAvulsionLocation=pcrgt(windowdiversity(self.AvulsionLocationOnChannel,celllength()*scalar(5)),scalar(1))
    # increase in elevation for all cells upstream of self.AvulsionLocationOnChannel, to prevent circular paths!!
    # catchment of starting point of new channel 
    CatchmentMV=self.catchmentDispflux(cover(self.AvulsionLocationOnChannel,boolean(0)))
    Catchment=cover(CatchmentMV,boolean(0))
    # close also diagonal channels to prevent an ldd crossing between two diagonal neighbouring cells
    SLdd=lddrepair(ldd(2))
    CellsNeighbouringSouthOfCatchment=pcrne(upstream(SLdd,scalar(pcrand(Catchment,pcrnot(self.AvulsionLocationOnChannel)))),scalar(0))
    self.CatchmentClosedAtDiagonalNeighbours=pcror(Catchment,CellsNeighbouringSouthOfCatchment)
    #report(self.CatchmentClosedAtDiagonalNeighbours,"CatchmentClosedAtDiagonalNeighbours")
    # create dem
    self.DemForNewChannelLdd=DemLdd-ifthenelse(pcrand(self.Channel, \
             pcrnot(AreaAroundAvulsionLocation)),scalar(2e1),scalar(0)) \
             + ifthenelse(self.CatchmentClosedAtDiagonalNeighbours,scalar(4e1),scalar(0))
    #report(self.DemForNewChannelLdd,"DemForNewChannelLdd")

 def avulsionLocationNew(self,Elevation, Y):
    ## note that this function uses the width of the channel belt at the avulsion
    ## location which is a bit counter intuitive
    ##Uniform=uniform(self.ChannelBeltEdge)
    ## boolean map with TRUE at avulsion location on channel belt edge, FALSE
    ## on channel belt edge and MV elsewhere
    #ALoc=pcreq(ifthen(self.Clone,mapmaximum(Uniform)),Uniform)
    # on channel belt distance to avulsion location at channel belt edge, MV elsewhere
    DistALoc=spread(cover(self.AvulsionLocation,ifthen(self.ChannelBelt,boolean(0))),scalar(0),scalar(1))
    report(DistALoc,'distaloc')
    # on channel distance to avulsion location at channel belt edge, MV elsewhere
    DistALocCentres=ifthen(self.Channel,DistALoc)
    # TRUE: cells on channel nearest to avulsion location on channel belt edge, FALSE:
    # rest of channel, MV elsewhere
    StartNewChs=pcreq(DistALocCentres,ifthen(self.Clone,mapminimum(DistALocCentres)))
    # in case of multiple cells with true on StartNewChs, take the upper one
    YStartNewChs=ifthen(StartNewChs,Y)
    # TRUE: avulsion location on channel, FALSE rest of map
    self.AvulsionLocationOnChannel=cover(pcreq(ifthen(self.Clone,mapminimum(YStartNewChs)),YStartNewChs),boolean(0))
    report(self.AvulsionLocationOnChannel,'Klaasje')
    # make valley to connect between avulsion loc and StartnewCh as small as possible
    # else the cb gets to high far away from an self.AvulsionLocation
    # first assign width of channel at avulsion location on channel to all cells on map
    WidthChannelOnAvulsionLocationOnChannel=ifthen(self.AvulsionLocationOnChannel,DistALoc)
    Tmp=cover(WidthChannelOnAvulsionLocationOnChannel,scalar(-1e31))
    WidthChannelEveryWhere=ifthenelse(defined(Y),mapmaximum(Tmp),mapmaximum(Tmp))
    WidthChannelEveryWhereFloat=cellValueMissingValueChecked(WidthChannelEveryWhere,1,1)
    RadiusChannelBeltInNumberCells=int(math.floor(WidthChannelEveryWhereFloat/cellValueMissingValueChecked(celllength(),1,1)))
    # added to prevent RadiusChannelBeltInNumberCells of -2934293434 or something
    if RadiusChannelBeltInNumberCells < 1:
      #print 'radius of channel belt got less than zero which is a bug'
      #print 'radius of channel in nr of cells was ', RadiusChannelBeltInNumberCells
      RadiusChannelBeltInNumberCells=1
    #print 'radius channelbelt ', RadiusChannelBeltInNumberCells
    self.AvulsionLocationsOnChannelMovedUp=self.AvulsionLocationOnChannel
    for i in range(0,RadiusChannelBeltInNumberCells-1):
      self.AvulsionLocationsOnChannelMovedUp=cover(self.orOfEdges(self.cover(self.endNode(self.AvulsionLocationsOnChannelMovedUp), \
                                                 boolean(0))), boolean(0))
    # avulsion location on channel should not be the inflow point, since splitting streams at
    # inflow point results in removing all streams when remove streams is called
    self.AvulsionLocationsOnChannelMovedUp=pcrand(self.AvulsionLocationsOnChannelMovedUp,pcrnot(self.InflowPoint)) 
    # if AvulsionLocationsOnChannelMovedUp does not contain a True (e.g. it was at the inflow point,
    # or avulsionLocationsOnChannel have moved out of the area, at the top of the map), take the
    # original avulsion location
    AvulsionLocationsOnChannelIsFalseEveryWhere=pcrnot(boolean(mapmaximum(scalar(self.AvulsionLocationsOnChannelMovedUp))))
    self.AvulsionLocationsOnChannelMovedUp=ifthenelse(AvulsionLocationsOnChannelIsFalseEveryWhere,self.AvulsionLocationOnChannel, \
                                           self.AvulsionLocationsOnChannelMovedUp)
    report(self.AvulsionLocationsOnChannelMovedUp,'Pietjes')
    # in case of multiple avulsionLocationsOnChannelMovedUp (joining streams just upstream of av.loc), take
    # the one closest to ALoc
    self.AvulsionLocationOnChannel=cover(pcreq(order(ifthen(self.AvulsionLocationsOnChannelMovedUp,DistALoc)),1), \
                                               boolean(0))
    report(self.AvulsionLocationOnChannel,'Pietje')
    # calculate distance between AvulsionLocationOnChannel and AvulsionLocation
    WidthChannelOnNewAvulsionLocationOnChannel=ifthen(self.AvulsionLocationOnChannel,DistALoc)
    Tmp=cover(WidthChannelOnNewAvulsionLocationOnChannel,scalar(-1e31))
    WidthChannelDiagonalEveryWhere=ifthenelse(defined(Y),mapmaximum(Tmp),mapmaximum(Tmp))
    IncreasedWidthChannelEveryWhere=WidthChannelDiagonalEveryWhere+(scalar(3.0)*celllength())+scalar(0.1)
    #print 'increased width channel ', cellValueMissingValueChecked(IncreasedWidthChannelEveryWhere,1,1)
    # then calculate the total dem, i.e. on the channel belt you add zero at the avulsion
    # location on the channel belt edge and an uphill zone in all directions (including the
    # avulsion location at the channel)
    DemLdd= Elevation+cover(ifthenelse(pcrgt(DistALoc,IncreasedWidthChannelEveryWhere),scalar(0),DistALoc),scalar(0))
    # OLD DemLdd= Elevation+cover(ifthenelse(pcrgt(DistALoc,0.75*MaxWidthBeltForAvulsionLocation),  \
    #             scalar(0),DistALoc),scalar(0))
    AreaAroundAvulsionLocation=pcrgt(windowdiversity(self.AvulsionLocationOnChannel,celllength()*scalar(5)),scalar(1))
    # increase in elevation for all cells upstream of self.AvulsionLocationOnChannel, to prevent circular paths!!
    # catchment of starting point of new channel 
    CatchmentMV=self.catchmentDispflux(cover(self.AvulsionLocationOnChannel,boolean(0)))
    Catchment=cover(CatchmentMV,boolean(0))
    # close also diagonal channels to prevent an ldd crossing between two diagonal neighbouring cells
    SLdd=lddrepair(ldd(2))
    CellsNeighbouringSouthOfCatchment=pcrne(upstream(SLdd,scalar(pcrand(Catchment, \
                                      pcrnot(self.AvulsionLocationOnChannel)))),scalar(0))
    self.CatchmentClosedAtDiagonalNeighbours=pcror(Catchment,CellsNeighbouringSouthOfCatchment)
    #report(self.CatchmentClosedAtDiagonalNeighbours,"CatchmentClosedAtDiagonalNeighbours")
    # create dem
    self.DemForNewChannelLdd=DemLdd-ifthenelse(pcrand(self.Channel, \
             pcrnot(AreaAroundAvulsionLocation)),scalar(2e1),scalar(0)) \
             + ifthenelse(self.CatchmentClosedAtDiagonalNeighbours,scalar(4e1),scalar(0))
    #report(self.DemForNewChannelLdd,"DemForNewChannelLdd")


 def catchmentDispflux(self,BooleanLocs):
   # gets a catchment of one or more boolean locations
   # is restricted to a certain number of diversions (here: 10)
   NPaths,NEPaths,EPaths,SEPaths,SPaths,SWPaths,WPaths,NWPaths=self.eightLdds()
   for x in range(0, 10):
     BL=cover(BooleanLocs,boolean(0))
     NCatch=catchment(NPaths,BL) 
     NECatch=catchment(NEPaths,BL) 
     ECatch=catchment(EPaths,BL) 
     SECatch=catchment(SEPaths,BL) 
     SCatch=catchment(SPaths,BL) 
     SWCatch=catchment(SWPaths,BL) 
     WCatch=catchment(WPaths,BL) 
     NWCatch=catchment(NWPaths,BL) 
     BooleanLocs=pcror(pcror(pcror(NCatch, NECatch), pcror(ECatch, SECatch)), \
                 pcror(pcror(SCatch, SWCatch), pcror(WCatch, NWCatch)))
   return BooleanLocs

 def eightLdds(self):
   NLddNoPit=self.removePitFromLdd(self.Arcs[0])
   NELddNoPit=self.removePitFromLdd(self.Arcs[1])
   ELddNoPit=self.removePitFromLdd(self.Arcs[2])
   SELddNoPit=self.removePitFromLdd(self.Arcs[3])
   SLddNoPit=self.removePitFromLdd(self.Arcs[4])
   SWLddNoPit=self.removePitFromLdd(self.Arcs[5])
   WLddNoPit=self.removePitFromLdd(self.Arcs[6])
   NWLddNoPit=self.removePitFromLdd(self.Arcs[7])
   WholeLdd=cover(cover(cover(cover(cover(cover(cover( NLddNoPit, NELddNoPit), ELddNoPit), \
            SELddNoPit), SLddNoPit), SWLddNoPit), WLddNoPit), NWLddNoPit)
   NPaths=lddrepair(cover(NLddNoPit,WholeLdd))
   NEPaths=lddrepair(cover(NELddNoPit,WholeLdd))
   EPaths=lddrepair(cover(ELddNoPit,WholeLdd))
   SEPaths=lddrepair(cover(SELddNoPit,WholeLdd))
   SPaths=lddrepair(cover(SLddNoPit,WholeLdd))
   SWPaths=lddrepair(cover(SWLddNoPit,WholeLdd))
   WPaths=lddrepair(cover(WLddNoPit,WholeLdd))
   NWPaths=lddrepair(cover(NWLddNoPit,WholeLdd))
   return NPaths,NEPaths,EPaths,SEPaths,SPaths,SWPaths,WPaths,NWPaths

 def removePitFromLdd(self,Ldd):
   LddNoPit=ifthen(pcrne(Ldd,ldd(5)),Ldd)
   return LddNoPit


 def diffusionMldd(self, SeaLevel, InputCubePerYear):

   self.ChannelsHeight=ifthenelse(pcrand(self.Bot,defined(self.ChannelsHeight)),SeaLevel,self.ChannelsHeight)

   # TIME BEING QStr has mv at outflow points, so has WidthChannelBelt
   # here I force a value at the outflow point for WidthChannelBeltMap
   WidthChannelBeltMapNoMV=cover(self.WidthChannelBeltMap, \
                           ifthen(pcrand(defined(self.ChannelsHeight),self.Bot),scalar(50.0)))
   # TIME BEING
   # width channel belt cannot be smaller than 50 m
   WidthChannelBeltMapNoMV=ifthenelse(pcrlt(WidthChannelBeltMapNoMV,scalar(50.0)),scalar(50.0),WidthChannelBeltMapNoMV)

   self.d_mldd.setDem(self.ChannelsHeight)                      # assign current DEM to mldd structure

   self.SedimentTransport=self.d_mldd.diffuse( \
                          InputCubePerYear,  \
                          WidthChannelBeltMapNoMV*celllength(), \
                          scalar(self.Bot), \
                          self.DiffusionStreams[0], \
                          self.DiffusionStreams[1], \
                          self.DiffusionStreams[2], \
                          self.DiffusionStreams[3], \
                          self.DiffusionStreams[4], \
                          self.DiffusionStreams[5], \
                          self.DiffusionStreams[6], \
                          self.DiffusionStreams[7], \
                          nominal(2000))

   # TIME BEING
   # very seldom, diffuse returns an mv
   # this is why this cover is used
   # a check is needed that prints an error message or stops the execution
   # when an mv occurs
   #self.ChannelsHeight=self.d_mldd.getDem()        # retrieve DEM from mldd structure
   self.ChannelsHeight=cover(self.d_mldd.getDem(),self.ChannelsHeight)        # retrieve DEM from mldd structure

 def setSlopeRatio(self,Elevation, X, Y):
   # Channel, X, Y are part of object in the model
   # Y increases from top to bottom!!
   # this goes wrong with streams that flow up!!!

   ChannelMV=ifthen(self.Channel,boolean(1))
   XOnChannel=ifthen(ChannelMV,X)
   YOnChannel=ifthen(ChannelMV,Y)
   CentreID=nominal(cover(uniqueid(ChannelMV),scalar(0)))
   SpreadZones=spreadzone(CentreID,scalar(0),scalar(1))

   XOnNearestChannelCell=areamaximum(cover(XOnChannel,-1e31),SpreadZones)
   YOnNearestChannelCell=areamaximum(cover(YOnChannel,-1e31),SpreadZones)

   AngleFirst=scalar(atan((X-XOnNearestChannelCell)/((YOnNearestChannelCell-Y)+0.00000000001)))

   Left=pcrle(X,XOnNearestChannelCell)
   Top=pcrle(Y,YOnNearestChannelCell)

   Angle=ifthenelse(pcrand(pcrnot(Left),pcrnot(Top)),(AngleFirst-scalar(180)),AngleFirst)
   AngleDir=directional(ifthenelse(pcrand(Left,pcrnot(Top)),AngleFirst+scalar(180),Angle))

   AngleDirDownStream=directional(ifthenelse(pcrle(X,XOnNearestChannelCell),scalar(AngleDir)-scalar(90), \
                      scalar(AngleDir)+scalar(90)))

   Slope=slope(Elevation)
   Aspect=aspect(Elevation)

   AngleDirMinusAspect=directional(scalar(AngleDir)-scalar(Aspect))
   SlopePerp=cos(AngleDirMinusAspect)*Slope

   AngleDirMinusAspectPlusNinety=directional(scalar(AngleDirDownStream)-scalar(Aspect))
   SlopeDown=cos(AngleDirMinusAspectPlusNinety)*Slope

   SlopeDownForRatio=ifthenelse(pcrlt(SlopeDown,0.0000001), scalar(0.0000001),SlopeDown)
   SlopeRatio=SlopePerp/SlopeDownForRatio

   self.SlopeRatioOnEdge=max(scalar(0.0),ifthen(self.ChannelBeltEdge,SlopeRatio))

 def setSlopeRatioCentre(self,Elevation, X, Y):
   # Channel, X, Y are part of object in the model
   # Y increases from top to bottom!!
   # this goes wrong with streams that flow up!!!

   ChannelMV=ifthen(self.Channel,boolean(1))
   XOnChannel=ifthen(ChannelMV,X)
   YOnChannel=ifthen(ChannelMV,Y)
   CentreID=nominal(cover(uniqueid(ChannelMV),scalar(0)))
   SpreadZones=spreadzone(CentreID,scalar(0),scalar(1))

   XOnNearestChannelCell=areamaximum(cover(XOnChannel,-1e31),SpreadZones)
   YOnNearestChannelCell=areamaximum(cover(YOnChannel,-1e31),SpreadZones)

   AngleFirst=scalar(atan((X-XOnNearestChannelCell)/((YOnNearestChannelCell-Y)+0.00000000001)))

   Left=pcrle(X,XOnNearestChannelCell)
   Top=pcrle(Y,YOnNearestChannelCell)

   Angle=ifthenelse(pcrand(pcrnot(Left),pcrnot(Top)),(AngleFirst-scalar(180)),AngleFirst)
   AngleDir=directional(ifthenelse(pcrand(Left,pcrnot(Top)),AngleFirst+scalar(180),Angle))

   AngleDirDownStream=directional(ifthenelse(pcrle(X,XOnNearestChannelCell),scalar(AngleDir)-scalar(90), \
                      scalar(AngleDir)+scalar(90)))

   Slope=slope(Elevation)
   Aspect=aspect(Elevation)

   AngleDirMinusAspect=directional(scalar(AngleDir)-scalar(Aspect))
   SlopePerp=cos(AngleDirMinusAspect)*Slope

   #AngleDirMinusAspectPlusNinety=directional(scalar(AngleDirDownStream)-scalar(Aspect))
   #SlopeDown=cos(AngleDirMinusAspectPlusNinety)*Slope
   SlopeDown=areamaximum(cover(self.MaxSlopeDeterministicStreamsMap,scalar(-10)),SpreadZones)

   # very low downstream slopes are allowed now
   SlopeDownForRatio=ifthenelse(pcrlt(SlopeDown,0.000000001), scalar(0.000000001),SlopeDown)
   SlopeRatio=SlopePerp/SlopeDownForRatio

   # to restrict extremely high slope ratios with downstream slopes close to zero (or zero)
   # min(..., scalar(8)) is used here. this should solve absence of avulsions during sea leve rise (?)
   # this is switched off again, that is why scalar(8) is now scalar(100)
   self.SlopeRatioOnEdge=min(max(scalar(0.0),ifthen(self.ChannelBeltEdge,SlopeRatio)),scalar(100.0))

 def probBifurcationSlopeRatio(self):
   # SlopeRatioProportionalityConstant is between 0 and 1
   SlopeRatioProportionalityConstant=scalar(inputs.SlopeRatioProportionalityConstant)
   # SlopeRatioExponent is between 1 and 5 (typically)
   SlopeRatioExponent=scalar(inputs.SlopeRatioExponent)
   ProbBifurcationSlopeRatioNeg=(self.SlopeRatioOnEdge*SlopeRatioProportionalityConstant)**SlopeRatioExponent
   self.ProbBifurcationSlopeRatio=max(min(ProbBifurcationSlopeRatioNeg,scalar(1.0)),scalar(0.0))

 def maxAnnualDischarge(self):
   # it seems the output does not have a s.d. of SDMaxAnnualDischarge, but
   # slightly smaller
   MapUniform=mapuniform()
   MeanMaxAnnualDischarge = scalar(10000.0)    # m3 per year
   SDMaxAnnualDischarge = scalar(1000.0)
   MaxDischargeSometimesNegative=MeanMaxAnnualDischarge+0.7796*SDMaxAnnualDischarge* \
                (scalar(0.0)-ln(scalar(0.0)-ln(MapUniform))-0.5772)
   self.MaxDischarge=max(MaxDischargeSometimesNegative,scalar(0.0))

 def dischargeRatio(self):
   ThresholdMaxDischargeForAvulsion=scalar(100000.0)
   self.DischargeRatio=self.MaxDischarge/ThresholdMaxDischargeForAvulsion

 def probBifurcationDischarge(self):
   # probBifurcationDischarge > 1, _/ shape (x discharge ration, y prob)
   # 0 < probBifurcationDischarge < 1, /- shape (x discharge ration, y prob)
   # typical range of probBifurcationDischarge is [1,5]
   # 1.3 is ok voor testing with small area (many avulsions)
   # 3.5 is ok voor eurodelta, en ook voor testruns nov 2004
   ProbAvulsionDischargeExponent=scalar(3.5)
   self.ProbBifurcationDischarge=min(self.DischargeRatio**ProbAvulsionDischargeExponent,scalar(1.0))
   if printToScreen:
     print 'prob. bifurcation discharge is ', cellValueMissingValueChecked((self.ProbBifurcationDischarge),0,0)
 
 def probBifurcationDischargeFixed(self):
   self.ProbBifurcationDischarge=ifthenelse(pcrlt(mapuniform(),scalar(inputs.ProbBifurcationDischarge)),scalar(1),scalar(0))
   if printToScreen:
     print 'flood for bifurcation occurs is (0 or 1): ', cellValueMissingValueChecked((self.ProbBifurcationDischarge),0,0)

 def probBifurcation(self):
   self.ProbBifurcation=min(self.ProbBifurcationDischarge*self.ProbBifurcationSlopeRatio,scalar(1.0))

 def avulsionLocations(self):
   Uniform=uniform(boolean(1))
   self.AvulsionLocations=self.ProbBifurcation > Uniform
   #if printToScreen:
   #  print 'uniform value is ', cellValueMissingValueChecked(Uniform,5,5)

 def avulsionLocationComponent(self,X,Y):
   # in case of multiple avulsion locations, take the upper one
   YAvulsionLocations=ifthen(self.AvulsionLocations,Y)
   AvulsionLocationSelectedOnY=pcreq(mapminimum(YAvulsionLocations),YAvulsionLocations)
   # in case of multiple avulsion locations (still), take the left one
   XAvulsionLocations=ifthen(AvulsionLocationSelectedOnY,X)
   # boolean map with TRUE at avulsion location on channel belt edge, FALSE
   # on channel belt edge and MV elsewhere
   self.AvulsionLocation=cover(pcreq(mapminimum(XAvulsionLocations),XAvulsionLocations), \
                         ifthen(defined(self.AvulsionLocations),boolean(0)))

 def avulsionOccurs(self):
   self.BifurcationOccursPythonInteger = cellValueMissingValueChecked(boolean(mapmaximum(scalar(self.AvulsionLocation))),0,0)

 def bifurcationLocationNew(self,Elevation,X,Y):
   self.probBifurcationDischargeFixed()
   slopeRatioNeedsToBeCalculated= saveExtraVariables or \
                                  (cellValueMissingValueChecked(self.ProbBifurcationDischarge,0,0) > 0.5) or \
                                  self.doSavePropertiesOfStreamsIndependentOfEvents
   if slopeRatioNeedsToBeCalculated:
     self.setSlopeRatioCentre(Elevation,X,Y)
     self.probBifurcationSlopeRatio()
     self.probBifurcation()
     self.avulsionLocations()
     self.avulsionLocationComponent(X,Y)
     self.avulsionOccurs()
      #self.maxAnnualDischarge()  not used anymore
      #self.dischargeRatio()  not used anymore
   else:
     self.BifurcationOccursPythonInteger = 0

 def checkOnCircularPathOrCore(self,NewChannelLdd,Catchment):
  #report(Catchment,"Catchment")
  NewChannelLddRepaired=cover(NewChannelLdd,ldd(5))
  Inflow=upstream(NewChannelLddRepaired,scalar(1))
  #report(Inflow,"Inflow")
  InflowOnCatchment=ifthenelse(Catchment,Inflow,scalar(0))
           # test InflowOnCatchment=ifthenelse(Catchment,ifthenelse(pcrgt(mapuniform(), 0.5),scalar(1),scalar(0)),scalar(0))
  #report(InflowOnCatchment,"InflowOnCatchment")
  InflowOnCatchmentInteger=cellValueMissingValueChecked(boolean(mapmaximum(InflowOnCatchment)),0,0)
  # pit of new channel not at bottom but somewhere in the middle (i.e. core)
  # check this also
  # this check is based upon:
  # NewChannelLdd has either 1) a pit at the bottom or
  # no pit at all (i.e. it flow into an existing channel)
  # in all other cases it is wrong
  Pit=cover(pcreq(NewChannelLdd,ldd(5)),boolean(0))
  PitOccursOutsideBottom=boolean(mapmaximum(scalar(pcrand(Pit,pcrnot(self.Bot)))))
  PitOccursOutsideBottomInteger=cellValueMissingValueChecked(PitOccursOutsideBottom,0,0)
  if printToScreen:
    print '## PitOccursOutsideBottomInteger is ', PitOccursOutsideBottomInteger

  # sometimes the bifurcation point on the channel is actually not on the channel
  # resulting in a newpath that does not start at a channel - check for this
  AvulsionLocationOnChannelIsActuallyOnChannel=boolean(mapmaximum(scalar(
                    pcrand(self.AvulsionLocationOnChannel,pcrne(self.ChannelsOrdinal,ordinal(0)))
                    )))
  AvulsionLocationOnChannelIsActuallyOnChannelInteger= \
                    cellValueMissingValueChecked(AvulsionLocationOnChannelIsActuallyOnChannel,0,0) 
  if printToScreen:
    print '## AvulsionLocationOnChannelIsActuallyOnChannelInteger is ', AvulsionLocationOnChannelIsActuallyOnChannelInteger

  # circular path, core, or avulsion location on channel is actually NOT on channel!
  # this means that bifurcation is not executed
  self.StoppedBifurcation=InflowOnCatchmentInteger or PitOccursOutsideBottomInteger  \
                                       or not(AvulsionLocationOnChannelIsActuallyOnChannelInteger)
  if printToScreen:
    print '## the bifurcation is stopped is ', self.StoppedBifurcation


 def newChannel(self,Dem,Bot,Left,Right,Top,StartPoint,WidthStream, Catchment):
   self.CircularPathOccurs=0
   # DJ LET OP: if ldds pit is not at edge, decrease depth at outflow area and be sure that
   # that is indeed the outflow area (not left or right edge, see Tim's model NITG)
   # Channels is an ordinal with Channel numbers 
   DemMod=ifthenelse(pcror(pcror(Left,Right),Top),Dem+50,ifthenelse(Bot,scalar(-1000),cover(Dem,scalar(-1000))))
   #report(DemMod,"demMod")
   Ldd=ifthenelse(Bot,ldd(5),lddcreate(DemMod,1e31,1e31,1e31,1e31))
   #report(Ldd,"ldd")
   Path=path(Ldd,StartPoint)
   PathLdd=ifthen(Path,Ldd)
   #report(PathLdd,"PathLdd")
   TTTmp=scalar(pcror(boolean(self.ChannelsOrdinal),StartPoint))
   #report(TTTmp,"tTTmp")
   TTmp=accuflux(PathLdd,TTTmp)
   #report(TTmp,"tTmp")
   Tmp=ifthen(pcreq(TTmp,scalar(1)),boolean(1))
   RealPath=cover(Tmp,boolean(0))
   newChannelLdd=ifthen(RealPath,Ldd)
   #report(newChannelLdd,"newChannelLdd")
   # check on circular paths or core
   self.checkOnCircularPathOrCore(newChannelLdd,Catchment)
   if printToScreen:
     print self.StoppedBifurcation
   if self.StoppedBifurcation:
     if printToScreen:
       print 'circular path or core occurs'
       print self.StoppedBifurcation
   else:
    self.BifurcationAndChannelID=succ(self.BifurcationAndChannelID)
    if printToScreen:
      print 'id of new channel (new version): ', cellValueMissingValueChecked(self.BifurcationAndChannelID,0,0)
    self.ChannelsOrdinal=ifthenelse(pcrand(RealPath,pcrnot(StartPoint)),self.BifurcationAndChannelID,self.ChannelsOrdinal)
    self.d_mldd.addStream(newChannelLdd)

 def removeStream(self):
   self.d_mldd.removeStream(
                              self.RemoveStreamDownstream[0],
                              self.RemoveStreamDownstream[1],
                              self.RemoveStreamDownstream[2],
                              self.RemoveStreamDownstream[3],
                              self.RemoveStreamDownstream[4],
                              self.RemoveStreamDownstream[5],
                              self.RemoveStreamDownstream[6],
                              self.RemoveStreamDownstream[7]
                              )
   # for reportsBifurcations (note that RemoveStreamDownstreamMap may have TRUE also when
   # streams are not removed, so ActualRemoveStreamDownstreamMap is needed to see when and which
   # bifurcations are really removed
   self.ActualRemoveStreamDownstreamMap=self.RemoveStreamDownstreamMap

 def allBifurcationLocs(self):
   self.AllBifurcationLocs=ifthenelse(self.AvulsionLocationOnChannel, self.BifurcationAndChannelID, self.AllBifurcationLocs)

 def allActiveBifurcationLocs(self):
   self.AllActiveBifurcationLocsBoolean=cover(
                                 pcrgt(self.sumOfEdges(self.conversionScalar(self.startNode())),scalar(1.0)),
                                 boolean(0))
   self.AllActiveBifurcationLocs=ifthenelse(self.AllActiveBifurcationLocsBoolean,
                                 self.AllBifurcationLocs,
                                 ordinal(0))

 def reportsBifurcations(self):

   if self.currentTimeStep == 1:
     # this is needed to put a mv map at timestep zero in the case
     # that the code below in this function does not generate a map at timestep
     # zero. This map is needed since timeinputsparse in avulsions.mod
     # needs a map at timestep 1
     MVMapBoolean=ifthen(pcrnot(self.Clone),boolean(0))
     MVMapOrdinal=ifthen(pcrnot(self.Clone),ordinal(0))
     MVMapNominal=ifthen(pcrnot(self.Clone),nominal(0))
     MVMapScalar=ifthen(pcrnot(self.Clone),scalar(0))
     report(MVMapOrdinal,generateNameST("ABL",self.currentLoop,self.currentTimeStep))
     report(MVMapOrdinal,generateNameST("AABL",self.currentLoop,self.currentTimeStep))
     report(self.ChannelsOrdinal, generateNameST("CO",self.currentLoop,self.currentTimeStep))
     report(MVMapBoolean,generateNameST("ARSDM",self.currentLoop,self.currentTimeStep))
     report(MVMapBoolean,generateNameST("AL",self.currentLoop,self.currentTimeStep))
     report(MVMapNominal,generateNameST("BE",self.currentLoop,self.currentTimeStep))
     report(MVMapScalar,generateNameST("SRS",self.currentLoop,self.currentTimeStep))

   if self.BifurcationOccursPythonInteger:
     # note that a new avulsion location on an existing one will overwrite it!
     self.allBifurcationLocs()
     report(self.AllBifurcationLocs,generateNameST("ABL",self.currentLoop,self.currentTimeStep))
     report(self.AvulsionLocation,generateNameST("AL",self.currentLoop,self.currentTimeStep))
     report(self.SlopeRatioOnEdge,generateNameST("SRS",self.currentLoop,self.currentTimeStep))

   if self.ActualRemoveStreamsOccursPythonInteger:
     report(self.ActualRemoveStreamDownstreamMap,generateNameST("ARSDM",self.currentLoop,self.currentTimeStep))

     # BifurcationToAvulsion is TRUE if one of the two channels downstream of a bifurcation
     # is removed, this can be the 'original' channel or the 'new' channel 
     BifurcationToAvulsion=pcrand(self.ActualRemoveStreamDownstreamMap,
                           pcrne(self.ChannelsOrdinal,ordinal(0)))

     # RealBifurcationToAvulsion is TRUE if the 'original' channel downstream of a bifurcation
     # is removed
     COAtDownstreamCells=self.endNode(self.ChannelsOrdinal)
     COAtDownstreamCellEqualsCOAtBifurcationLocation=self.eq(COAtDownstreamCells,self.AllActiveBifurcationLocs)
     COAtDownstreamCellEqualsCOAtBifurcationLocationCovered=self.cover(
                                                            COAtDownstreamCellEqualsCOAtBifurcationLocation,boolean(0))
     RealBifurcationToAvulsion=pcrand(self.orOfEdges(COAtDownstreamCellEqualsCOAtBifurcationLocationCovered),
                                      BifurcationToAvulsion)

     # BifurcationRemoved is TRUE if the channels downstream and upstream of an bifurcation
     # are removed (due to a change in bifurcations upstream, ie a whole branch is removed)
     BifurcationRemoved=pcrand(self.ActualRemoveStreamDownstreamMap, 
                        pcrnot(pcrne(self.ChannelsOrdinal,ordinal(0))))

     # BifurcationEnd exists when one of the bifurcations is removed and has the code
     # 0 - no bifurcation existed or bifurcation is not removed
     # 1 - bifurcation is removed due to change in bifurcations upstream
     # 2 - bifurcation is removed due to removal of newer stream (i.e. avulsion not succesfull
     # 3 - bifurcation is removed due to removal of older stream (i.e. a real avulsion)
     # the following two lines are old stuff, BifurcationRemoved does not work
     # so now everything that is NOT BifurcationToAvulsion or RealBifurcationToAvulsion
     # is a 1
     #BifurcationEnd=ifthenelse(BifurcationRemoved,nominal(1),nominal(0))
     #BifurcationEnd=ifthenelse(BifurcationToAvulsion,nominal(2),BifurcationEnd)
     BifurcationEnd=ifthenelse(BifurcationToAvulsion,nominal(2),nominal(1))
     BifurcationEnd=ifthenelse(RealBifurcationToAvulsion,nominal(3),BifurcationEnd)
     report(BifurcationEnd,generateNameST("BE",self.currentLoop,self.currentTimeStep))

   if self.BifurcationOccursPythonInteger or self.ActualRemoveStreamsOccursPythonInteger:
     self.allActiveBifurcationLocs()
     report(self.AllActiveBifurcationLocs,generateNameST("AABL",self.currentLoop,self.currentTimeStep))
     report(self.ChannelsOrdinal, generateNameST("CO",self.currentLoop,self.currentTimeStep))

   #report(self.TotalQStreams,generateNameST("TQS",self.currentLoop,self.currentTimeStep))
   if self.BifurcationOccursPythonInteger or self.ActualRemoveStreamsOccursPythonInteger  \
              or ((self.currentTimeStep % reportIntervalQFractionStreams) == 0) or (self.currentTimeStep == 1):
     report(min(self.minOfEdges(self.SqrtSlopeFractionStreams),scalar(1)), generateNameST("QFS",self.currentLoop,self.currentTimeStep))


class DiffScript(ModelScriptTemplate):

 def avulsionPlots(self, directory, InflowPointX):
   bifurcation.bifurcationStatistics(directory, nrOfRows,InflowPointX)
   windowLengthTimeSteps=1000.0
   bifstats.bifurcationStatistics(directory,nrOfRows, nrOfCols, nrOfTimeSteps,windowLengthTimeSteps)
   bifstats.slicerPalette(directory,nrOfRows, nrOfCols, nrOfTimeSteps,windowLengthTimeSteps)

 def __init__(self,argv):
  # nr loops and nr timesteps are the two last arguments
  ModelScriptTemplate.__init__(self,argv,nrOfSamples,nrOfTimeSteps,'clone.map')
  # tracing aanzetten
  self.setTrace(1)
  # set to 1 for starting multiple samples at once on one machine
  self.setForkSamples(0)
  if not printToScreen:
    self.setQuiet(True)
    self.setTrace(False)

 def saveEdges(self, firstPartFileName, Edges):
  extension = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']
  for i in range(len(Edges)):
   report(Edges[i],  self.generateName(firstPartFileName + extension[i]))

 def savePropertiesOfStreams(self):
  self.d_digraph.calculationsForSafePropertiesStreams()
  report(self.d_digraph.QStr,self.generateName("QStr"))
  report(ifthen(self.d_digraph.ChannelBelt,boolean(1)), self.generateName("Belt"))
  report(self.d_digraph.ChannelBeltOrdinal,self.generateName("CBO"))
  report(scalar(self.d_digraph.TotalProbabilityForBifurcation),self.generateName("TPFB"))
  if saveExtraVariables:
    report(self.d_digraph.DepthChannelMap,self.generateName("DC"))
    report(self.d_digraph.VelocityChannelsMap,self.generateName("VC"))
    report(self.d_digraph.WidthChannelMap,self.generateName("WC"))
    report(self.d_digraph.WidthChannelBeltMap, self.generateName("WCB"))
    report(self.d_digraph.ChannelBeltEdge, self.generateName("CBE"))
    report(self.d_digraph.ChannelBeltEdgeOnChannelBelt, self.generateName("CBEOCB"))
    report(self.d_digraph.SedimentTransport,self.generateName("Tr"))
    report(self.d_digraph.MaxWidthChannelBeltMap,self.generateName("MWCB"))
    report(self.d_digraph.DepthAtChannelBelt,self.generateName("DAC"))
    

 def saveMiscResults(self):
  report(self.d_sampleData.Elevation,self.generateName("EL"))
  report(self.d_sampleData.Erosion,self.generateName("Er"))
  if saveExtraVariables:
    report(self.d_digraph.SlopeRatioOnEdge,self.generateName("SROE"))
    report(self.d_digraph.ProbBifurcationSlopeRatio,self.generateName("PASR"))
    report(self.d_digraph.ProbBifurcation,self.generateName("PA"))
    report(self.d_digraph.ChannelsHeight,self.generateName("CHA"))

 def avulsionStatistics(self, InflowPointX):
   for sample in range(1,nrOfSamples+1):
     #command = 'pcrcalc -r ' + str(sample) + ' --noheader -f avulsions.mod ' + str(nrOfTimeSteps)
     #os.system(command)
     execute("pcrcalc -r %d --noheader -f avulsions.mod %d" % (sample, nrOfTimeSteps))
     self.avulsionPlots(str(sample),InflowPointX)
     commandOne = 'ps2pdf ' + str(sample) + '/avulsions.ps ' + str(sample) + '/avulsions.pdf'
     commandTwo = 'ps2pdf ' + str(sample) + '/bifFrequency.ps ' + str(sample) + '/bifFrequency.pdf'
     os.system(commandOne)
     os.system(commandTwo)

 def outputTimeSeries(self):
   for sample in range(1,nrOfSamples+1):
     command = 'pcrcalc -r ' + str(sample) + ' -f outputTimeSeries.mod ' + str(nrOfTimeSteps)
     os.system(command)


 def premcloop(self):

  self.d_modelData = ModelData()


 def initial(self):

   global BeltsHeight


   self.d_sampleData = SampleData(self.d_modelData.clone(), self.d_modelData.X, self.d_modelData.Y, \
                       self.d_modelData.InflowPoint, nrOfTimeSteps, self.d_modelData.Bot, self.d_modelData.Top, \
                       diffusionParameter)

   report(self.d_sampleData.TectMovTilt,"TMT")
   report(self.d_sampleData.TectMovFau,"TMF")

   ## TIME BEING
   ## pleurt er een flinke laag op zodat ie nooit de base insnijdt
   self.d_sampleData.d_block.add(ifthen(self.d_sampleData.d_clone,scalar(50)), \
                                 ifthen(self.d_sampleData.d_clone,scalar(10.0)), \
                                 ifthen(self.d_sampleData.d_clone,nominal(0)))
   self.d_sampleData.Elevation=self.d_sampleData.d_block.surface()
   # TIME BEING old elevation
   self.d_sampleData.ElevationOld=self.d_sampleData.Elevation
   ## END TIME BEING
   ## END TIME BEING

   self.d_sampleData.generateAllTimeseries()

   PitsLdd=first.newLddWithPits(self.d_modelData.clone())

   self.d_digraph=Digraph(PitsLdd, PitsLdd, PitsLdd, PitsLdd, PitsLdd, PitsLdd, PitsLdd, PitsLdd, \
                  self.d_modelData.InflowPoint, \
                  self.d_modelData.Bot, self.d_modelData.Top, self.d_modelData.Duration, self.d_modelData.MapEdges)

   # initial channel
   self.d_digraph.newChannel(self.d_sampleData.Elevation,self.d_modelData.Bot,self.d_modelData.Left, \
     self.d_modelData.Right,self.d_modelData.Top, self.d_modelData.InflowPoint, \
     self.d_modelData.WidthStream, \
     ifthenelse(self.d_modelData.d_clone,boolean(0),boolean(0)))

   self.d_digraph.getStream()
   self.d_digraph.setCreationYearStreams(self.currentTimeStep()*self.d_modelData.Duration)

   self.d_digraph.setPropertiesChannels(self.d_sampleData.Elevation,self.d_modelData.InflowPoint, \
                          self.d_sampleData.InitialWaterInputCubePerSecond,self.d_sampleData.diffusionParameter,1,0)

   # replace mv at inflow point with an ordinal 1, since newChannels generates mv at startpoint
   self.d_digraph.ChannelsOrdinal=ifthenelse(self.d_modelData.InflowPoint,ordinal(1),self.d_digraph.ChannelsOrdinal) 

   # replace mv at inflow point with a width stream, since newChannels generates mv at startpoint
   self.d_digraph.ChannelsHeight=ifthen(pcrne(self.d_digraph.ChannelsOrdinal,ordinal(0)),self.d_sampleData.Elevation)


   BeltsHeight=dyn.beltsHeight(self.d_digraph.ChannelBelt,self.d_digraph.ChannelsHeight)
   self.d_digraph.YearsSinceAvulsion=0.0


 def dynamic(self):

   global BeltsHeight,  TotFlux

   self.d_digraph.doSavePropertiesOfStreamsIndependentOfEvents = ((self.currentTimeStep() % reportInterval) == 0) \
                               or self.currentTimeStep() == 1

   self.d_digraph.YearsSinceAvulsion=self.d_digraph.YearsSinceAvulsion+1.0
   self.d_digraph.currentTimeStep = self.currentTimeStep()
   self.d_digraph.currentLoop = self.currentSample()

   TimeYears = self.currentTimeStep()*self.d_modelData.Duration+self.d_modelData.Duration

   self.d_digraph.bifurcationLocationNew(self.d_sampleData.Elevation,self.d_modelData.X,self.d_modelData.Y)

   WaterInputCubePerSecond=timeinputscalar("water.tss",self.d_sampleData.d_clone)

   if self.d_digraph.BifurcationOccursPythonInteger:
      if printToScreen:
        print 'BIFURCATION OCCURS'
      self.d_digraph.setPropertiesChannels(self.d_digraph.ChannelsHeight,self.d_modelData.InflowPoint,WaterInputCubePerSecond, \
                                           self.d_sampleData.diffusionParameter,1,0)
      self.d_digraph.avulsionLocationNew(self.d_sampleData.Elevation, self.d_modelData.Y)

      self.d_digraph.newChannel(
                                self.d_digraph.DemForNewChannelLdd, \
                                self.d_modelData.Bot, \
                                self.d_modelData.Left, \
                                self.d_modelData.Right, \
                                self.d_modelData.Top, \
                                self.d_digraph.AvulsionLocationOnChannel, \
                                self.d_modelData.WidthStream, \
                                self.d_digraph.CatchmentClosedAtDiagonalNeighbours)
      if self.d_digraph.StoppedBifurcation:
        if printToScreen:
          print 'circular path or core occurs'
        self.d_digraph.BifurcationOccursPythonInteger=0
      else:
        self.d_digraph.getStream()
        self.d_digraph.setCreationYearStreams(self.currentTimeStep()*self.d_modelData.Duration+self.d_modelData.Duration)
        self.d_digraph.YearsSinceAvulsion=0.0

        self.d_digraph.ChannelsHeight=dyn.channelsHeight(self.d_digraph.ChannelsHeight, \
                                      self.d_digraph.ChannelsOrdinal, self.d_sampleData.Elevation)

   self.d_sampleData.updateActiveChannelsAgeYears(self.d_digraph.ChannelsOrdinal)

   # this has to be done before diffusionMldd else diffusion is not up to date
   # also always with bifurcation
   #calculatePropertiesOfStreams = ((self.currentTimeStep() % 4) == 0) \
   #                               or self.d_digraph.BifurcationOccursPythonInteger \
   #                               or (self.currentTimeStep() == 0) \
   #                               or (self.d_digraph.YearsSinceAvulsion < 10.0)

   # this can be removed, later, since this actually means that calculatePropertiesOfStreams
   # is always True. If this is removed, the if calculatePropertiesOfStreams below can also
   # be removed
   #calculatePropertiesOfStreams = ((self.currentTimeStep() % 1) == 0) \
   #                               or self.d_digraph.BifurcationOccursPythonInteger \
   #                               or (self.currentTimeStep() == 0) \
   #                               or (self.d_digraph.YearsSinceAvulsion < 10.0)

   runChannelsGeometry= ((self.currentTimeStep() % 4) == 0) \
                        or self.d_digraph.BifurcationOccursPythonInteger \
			or (self.currentTimeStep() == 0) \
			or (self.d_digraph.YearsSinceAvulsion < 10.0)
 
   if self.d_digraph.BifurcationOccursPythonInteger:
     tryToRemoveStreams = 0
   else:
     tryToRemoveStreams = 1

   self.d_digraph.ActualRemoveStreamsOccursPythonInteger = 0

   self.d_digraph.setPropertiesChannels(self.d_digraph.ChannelsHeight,self.d_modelData.InflowPoint, WaterInputCubePerSecond, \
                                        self.d_sampleData.diffusionParameter,runChannelsGeometry,tryToRemoveStreams)
   self.d_digraph.ActualRemoveStreamsOccursPythonInteger = self.d_digraph.RemoveStreamsOccursPythonInteger \
                                                              and (self.d_digraph.YearsSinceAvulsion > 0.0)
   if self.d_digraph.ActualRemoveStreamsOccursPythonInteger:
     if printToScreen:
       print 'CHANNELS(S) REMOVED'
     self.d_digraph.removeStream()
     self.d_digraph.getStream()
     self.d_digraph.setCreationYearStreams(self.currentTimeStep()*self.d_modelData.Duration+self.d_modelData.Duration)
     self.d_digraph.setPropertiesChannels(self.d_digraph.ChannelsHeight,self.d_modelData.InflowPoint, WaterInputCubePerSecond, \
                                          self.d_sampleData.diffusionParameter,1,0)
     self.d_digraph.updateChannelsOrdinalAfterRemovingChannels()
     # TIME BEING is this needed?
     self.d_digraph.ChannelsHeight=dyn.channelsHeight \
                 (self.d_digraph.ChannelsHeight, self.d_digraph.ChannelsOrdinal, self.d_sampleData.Elevation)

   SeaLevel=timeinputscalar("sl.tss",self.d_sampleData.d_clone)

   UpstreamInputCubePerYear=timeinputscalar("material.tss",self.d_sampleData.d_clone)
   UpstreamInputCubePerYearAtInflowPoint=ifthenelse(self.d_modelData.InflowPoint, scalar(UpstreamInputCubePerYear),scalar(0))

   # calculate height of channels and sediment transport of channels with diffusion
   InputCubePerYear=UpstreamInputCubePerYearAtInflowPoint+self.d_sampleData.InputChannelsCubePerYear
   self.d_digraph.diffusionMldd(SeaLevel, InputCubePerYear )

   if (self.currentTimeStep() == 1):
     self.d_digraph.ChannelsHeightBeforeOverbankDepositionCalculation=self.d_digraph.ChannelsHeight

   if (self.currentTimeStep() % 10) == 0 or self.d_digraph.BifurcationOccursPythonInteger or (self.currentTimeStep() == 1):
     BeltsHeight=cover(self.d_digraph.ChannelsHeight, \
                 dyn.beltsHeightWithSpread(self.d_digraph.ChannelBelt,self.d_digraph.ChannelsHeight))
     self.d_sampleData.ElevationForCalculationOverbankDeposition= \
                 cover(self.d_digraph.ChannelsHeightBeforeOverbankDepositionCalculation, self.d_sampleData.Elevation)
     self.d_sampleData.DepositionBelts=cover(BeltsHeight-self.d_sampleData.ElevationForCalculationOverbankDeposition,scalar(0.0))

     #self.d_sampleData.depositionOverbankWindowOld(self.d_modelData.CF, self.d_modelData.BF, self.d_digraph.ChannelBelt, \
     #              self.d_digraph.ChannelBeltEdgeOnChannelBelt)
     self.d_sampleData.depositionOverbankWindowNew(self.d_modelData.CF, self.d_modelData.BF, self.d_digraph.ChannelBelt, \
                   self.d_digraph.ChannelBeltEdgeOnChannelBelt)
     # note this does not take into account a single cell width channel that is created
     # inbetween two calculations of overbank deposition (i.e. the old height is taken there of the
     # current timestep instead of the real old height
     # this is dealt now to call this always if self.d_digraph.BifurcationOccursPythonInteger!!
     self.d_digraph.ChannelsHeightBeforeOverbankDepositionCalculation=self.d_digraph.ChannelsHeight

     self.d_sampleData.Elevation=cover(self.d_digraph.ChannelsHeight,cover(BeltsHeight,self.d_sampleData.Elevation)+ \
                                 cover(self.d_sampleData.DepositionOverbank,scalar(0)))
     if saveExtraVariables:
       report(self.d_sampleData.DepositionOverbank,self.generateName("DO"))
       report(self.d_sampleData.Lower,self.generateName("L"))
   self.d_sampleData.erosion(self.d_digraph.ChannelsOrdinal, self.d_digraph.ChannelBelt, SeaLevel)


   doSavePropertiesOfStreams = self.d_digraph.doSavePropertiesOfStreamsIndependentOfEvents \
                               or self.d_digraph.BifurcationOccursPythonInteger \
                               or self.d_digraph.ActualRemoveStreamsOccursPythonInteger \

   if doSavePropertiesOfStreams:
      self.savePropertiesOfStreams()
      self.saveMiscResults()

   # reports for bifurcations and resulting statistics
   self.d_digraph.reportsBifurcations()


   # block, old version without reading from block

   # TIME BEING added diagonals to channel belt
   BeltForBlock=pcror(pcrge(window4total(scalar(self.d_digraph.ChannelBelt)) , 2) , self.d_digraph.ChannelBelt)
   BeltOrdinalForBlock=cover(ifthenelse(BeltForBlock,
                       windowmajority(ifthen(self.d_digraph.ChannelBelt,self.d_digraph.ChannelBeltOrdinal),3*celllength())
                       ,ordinal(0)),ordinal(0))
   DepthAtChannelBeltForBlock=cover(windowaverage(self.d_digraph.DepthAtChannelBelt,3*celllength()),
                              self.d_digraph.DepthAtChannelBelt)
   # END TIME BEING added diagonals to channel belt

   ErosionSurface=self.d_sampleData.Elevation-ifthenelse(BeltForBlock,DepthAtChannelBeltForBlock,scalar(0.0))
   Erosion=max(self.d_sampleData.ElevationOld-ErosionSurface,scalar(0.0))
   self.d_sampleData.d_block.remove(Erosion)
   AddedThickness=ifthenelse(BeltForBlock,DepthAtChannelBeltForBlock, max(scalar(0.0), \
                    self.d_sampleData.Elevation-self.d_sampleData.ElevationOld))
   BeltOrdinalForBlockNominal=nominal(BeltOrdinalForBlock)
   self.d_sampleData.d_block.add(AddedThickness, ifthen(self.d_sampleData.d_clone,scalar(10.0)),
                    BeltOrdinalForBlockNominal)
   #self.d_sampleData.d_block.add(AddedThickness, ifthen(self.d_sampleData.d_clone,scalar(10.0)),
   #                 ifthen(self.d_modelData.d_clone,nominal(self.currentTimeStep())))
   self.d_sampleData.ElevationOld=self.d_sampleData.Elevation


#   # block, new version, this should work.. (needs to be tested)
#
#   # TIME BEING added diagonals to channel belt
#   BeltForBlock=pcror(pcrge(window4total(scalar(self.d_digraph.ChannelBelt)) , 2) , self.d_digraph.ChannelBelt)
#   BeltOrdinalForBlock=cover(ifthenelse(BeltForBlock,
#                       windowmajority(ifthen(self.d_digraph.ChannelBelt,self.d_digraph.ChannelBeltOrdinal),3*celllength())
#                       ,ordinal(0)),ordinal(0))
#   DepthAtChannelBeltForBlock=cover(windowaverage(self.d_digraph.DepthAtChannelBelt,3*celllength()),
#                              self.d_digraph.DepthAtChannelBelt)
#   # END TIME BEING added diagonals to channel belt
#
#   ErosionSurface=self.d_sampleData.Elevation-ifthenelse(BeltForBlock,DepthAtChannelBeltForBlock,scalar(0.0))
#   ### TEST
#   #BlockDem=self.d_sampleData.d_block.surface()
#   #report(BlockDem,self.generateName("BD"))
#   #report(self.d_sampleData.ElevationOld,self.generateName("NBD"))
#   #report(self.d_sampleData.ElevationOld-BlockDem,self.generateName("DifD"))
#   ### END TEST
#   self.d_sampleData.ElevationOld=self.d_sampleData.d_block.surface()
#   Erosion=max(self.d_sampleData.ElevationOld-ErosionSurface,scalar(0.0))
#   self.d_sampleData.d_block.remove(Erosion)
#   AddedThickness=ifthenelse(BeltForBlock,DepthAtChannelBeltForBlock, max(scalar(0.0), \
#                    self.d_sampleData.Elevation-self.d_sampleData.ElevationOld))
#   BeltOrdinalForBlockNominal=nominal(BeltOrdinalForBlock)
#   self.d_sampleData.d_block.add(AddedThickness, ifthen(self.d_sampleData.d_clone,scalar(10.0)),
#                    BeltOrdinalForBlockNominal)
#   #self.d_sampleData.d_block.add(AddedThickness, ifthen(self.d_sampleData.d_clone,scalar(10.0)),
#   #                 ifthen(self.d_modelData.d_clone,nominal(self.currentTimeStep())))
#
#   # TIME BEING
#   #self.d_sampleData.d_block.changeBase(self.d_sampleData.TectMovTilt)
#   # END TIME BEING
#   self.d_sampleData.Elevation=self.d_sampleData.d_block.surface()







   if self.currentTimeStep() == nrOfTimeSteps:
     self.d_sampleData.d_block.resample(0.4)
     self.d_sampleData.d_block.saveAsBinary()
     print 'block is saved'
     for file in glob.glob('sediment.*'):
       if os.path.isfile(file):
         shutil.move(file,str(self.d_digraph.currentLoop))

 def postmcloop(self):
   self.outputTimeSeries()
   self.avulsionStatistics(self.d_modelData.InflowPointX)






# defines the name of the script (here DiffScript)
script = DiffScript(sys.argv)
# runs the script
sys.exit(script.run(True))
