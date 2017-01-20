import sys
from pcraster import *
import pcr
import first 
import dyn

# EDIT van pcr_python.py zoals van DEREK gekregen

# TODO
#  check of we _-setattr__ __call__ en type functie
#  wat typechecking kunnen doen voor onderscheid tussen
#  Raster en EightwayMap
class EightWayMap:
  # static (in C++ sense) for each object
  def staticInit(self):
    self.text2lddCode = {
      # 'P' :5, # pit, NIET IN INDEX
      'N' :8, # wind directions:
      'NE':9,
      'E' :6,
      'SE':3,
      'S' :2,
      'SW':1,
      'W' :4,
      'NW':7 }
    self.lddCodes = [1,2,3,4,6,7,8,9]

  def __init__(self):
    staticInit(self)
    # self.data = 8 kaarten van type bla bla

  # voor aanroep kaart[8] of kaart['N']
  def __getitem__(self, key):
    # als key 
    #  integer dan moet het in lddCodes zitten
    #  string  gebruikt text2lddCode
    return self.data[key]

  # kan ook per richting een functie
  def N(self):
    return self.__getitem__(self,'N')

  def __setitem__(self, key,value):
    # als nog geen is geinitialiseerd
    # dan uit value de vs afleiden
    # dan check of self.vs == value.vs
    self.data[key]=value

  def sum(self):
    # check nieuw lambda/apply spul of dit korten kan
    # return sum of all
    assert(False)
    pass


def eachWay():
   return [1,2,3,4,6,7,8,9]

# KAN dus worden
def upstreamsComponent(Input,Scal):
    # sum all weights
    Sum = Scal.sum()
    A   = EighWayMap()
    for l in eachWay(): # kar er wrsl ook uit m.b.v. lambda
      A[l]=upstreams_amounts(Scal[l],Sum,Input)
    return A

# WAS
def upstreamsComponent(Input,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal):
    # sum all weights
    Sum=NScal+NEScal+EScal+SEScal+SScal+SWScal+WScal+NWScal
    # amounts to be transported
    NA=upstreams_amounts(NScal,Sum,Input)
    NEA=upstreams_amounts(NEScal,Sum,Input)
    WA=upstreams_amounts(WScal,Sum,Input)
    EA=upstreams_amounts(EScal,Sum,Input)
    SEA=upstreams_amounts(SEScal,Sum,Input)
    SA=upstreams_amounts(SScal,Sum,Input)
    SWA=upstreams_amounts(SWScal,Sum,Input)
    WA=upstreams_amounts(WScal,Sum,Input)
    NWA=upstreams_amounts(NWScal,Sum,Input)
    return NA, NEA, EA, SEA, SA, SWA, WA, NWA

# EINDE Eightway code
#####################################################

# roep save aan met pcraster.save

def upstreams_amounts(Scal,Sum,Input):
    NA=(Scal/Sum)*Input
    return mapifelse(defined(boolean(NA)),NA,scalar(0.0))

def upstreams(Input,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal):
    # Note: if all Scal are zero this might go wrong
    NA, NEA, EA, SEA, SA, SWA, WA, NWA = \
                      upstreamsComponent(Input,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal)
    # create ldd's in all directions
    LddN=ldd(nominal(8))
    LddNE=ldd(nominal(9))
    LddE=ldd(nominal(6))
    LddSE=ldd(nominal(3))
    LddS=ldd(nominal(2))
    LddSW=ldd(nominal(1))
    LddW=ldd(nominal(4))
    LddNW=ldd(nominal(7))
    # transport per direction
    NTr=upstream(LddN,NA)
    NETr=upstream(LddNE,NEA)
    ETr=upstream(LddE,EA)
    SETr=upstream(LddSE,SEA)
    STr=upstream(LddS,SA)
    SWTr=upstream(LddSW,SWA)
    WTr=upstream(LddW,WA)
    NWTr=upstream(LddNW,NWA)
    # new state
    return NTr+NETr+ETr+SETr+STr+SWTr+WTr+NWTr

def dispflux(Input,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal):
    x=0
    Tmp=Input
    Flux=Input
    while x < 200:
      x = x+1
      Tmp = upstreams(Tmp,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal)
      Flux=Flux+Tmp
      print('displfux loop: {}'.format(x))
    return Flux

def dispfluxperdir(Input,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal):
    # note that all arguments should be greater than zero
    x=0
    Tmp=Input
    Flux=Input
    NTotFlux=scalar(0)
    NETotFlux=scalar(0)
    ETotFlux=scalar(0)
    SETotFlux=scalar(0)
    STotFlux=scalar(0)
    SWTotFlux=scalar(0)
    WTotFlux=scalar(0)
    NWTotFlux=scalar(0)
    while x < 200:
      x = x+1
      NFlux, NEFlux, EFlux, SEFlux, SFlux, SWFlux, WFlux, NWFlux = upstreamsComponent(Tmp,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal)
      NTotFlux=NTotFlux+NFlux
      NETotFlux=NETotFlux+NEFlux
      ETotFlux=ETotFlux+EFlux
      SETotFlux=SETotFlux+SEFlux
      STotFlux=STotFlux+SFlux
      SWTotFlux=SWTotFlux+SWFlux
      WTotFlux=WTotFlux+WFlux
      NWTotFlux=NWTotFlux+NWFlux
      Tmp = upstreams(Tmp,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal)
      xFormat = '%3d' % (x)
      pcr.write(3 * '\b')
      pcr.write(xFormat)
      sys.stdout.flush()
    # TotFlux is idem to results of dispflux(apart from pits)
    TotFlux = NTotFlux + NETotFlux + ETotFlux + SETotFlux + STotFlux + SWTotFlux + WTotFlux + NWTotFlux
    return TotFlux, NTotFlux, NETotFlux, ETotFlux, SETotFlux, STotFlux, SWTotFlux, WTotFlux, NWTotFlux

def slopedownstream(Ldd,Elevation):
    return cover((Elevation-downstream(Ldd,Elevation))/downstreamdist(Ldd),scalar(0.0))
    #return cover((Elevation-downstream(Ldd,Elevation))/downstreamdist(Ldd),Mask)

def slopedownstreamOpti(Ldd,Elevation,AreaOfInterestZero):
    return cover((Elevation-downstream(Ldd,Elevation))/downstreamdist(Ldd),AreaOfInterestZero)

def slopedownstreamOptiOpti(Ldd,Elevation,AreaOfInterestZero,DownstreamDist):
    return cover((Elevation-downstream(Ldd,Elevation))/DownstreamDist,AreaOfInterestZero)

def upstreamsperdir(NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal,LddN,LddNE,LddE,LddSE,LddS,LddSW,LddW,LddNW):
    # transport per direction
    NTr=upstream(LddN,NScal)
    NETr=upstream(LddNE,NEScal)
    ETr=upstream(LddE,EScal)
    SETr=upstream(LddSE,SEScal)
    STr=upstream(LddS,SScal)
    SWTr=upstream(LddSW,SWScal)
    WTr=upstream(LddW,WScal)
    NWTr=upstream(LddNW,NWScal)
    # new state
    return NTr+NETr+ETr+SETr+STr+SWTr+WTr+NWTr

#def upstreamsperdir(NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal,LddN,LddNE,LddE,LddSE,LddS,LddSW,LddW,LddNW):
#    # transport per direction
#    NTr=cover(upstream(LddN,NScal),scalar(0.0))
#    NETr=cover(upstream(LddNE,NEScal),scalar(0.0))
#    ETr=cover(upstream(LddE,EScal),scalar(0.0))
#    SETr=cover(upstream(LddSE,SEScal),scalar(0.0))
#    STr=cover(upstream(LddS,SScal),scalar(0.0))
#    SWTr=cover(upstream(LddSW,SWScal),scalar(0.0))
#    WTr=cover(upstream(LddW,WScal),scalar(0.0))
#    NWTr=cover(upstream(LddNW,NWScal),scalar(0.0))
#    # new state
#    return NTr+NETr+ETr+SETr+STr+SWTr+WTr+NWTr



def sedflow(PreviousHeight,InputCubePerYear, Diffusion, WidthStream, FixedHead, Duration, Discretisation, NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd):
    # PreviousHeight has mv elsewhere
    # Diffusion in (m3/m)/yr, volume per m width per year
    # Duration, time (yr) represented by one call
    # discretisation of Duration (nr of subtimesteps) 
    # LET OP: fixed head ook verdelen over discretisations!!
    # LET OP: NLdd, NELdd etc kan er volgens mij uit

    TimeStep=Duration/Discretisation
    # input per year (m/year)
    Input=InputCubePerYear/cellarea()
    # input per subtimestep (m/time step)
    InputTS=Input*TimeStep

    # transport (m3/year) for slope is 1
    KCuub=WidthStream*Diffusion
    # transport (m/year) for slope is 1
    KM=KCuub/cellarea()
    # transport (m/subtimestep) for slope is 1
    KTS=KM*TimeStep 

    # expected slope (-)
    SlExp=mapif(boolean(PreviousHeight),mapmaximum(cover(InputTS,scalar(-1000.0))))/KTS
    #print SlExp
 
    # create ldd's in all directions
    LddN=ldd(nominal(8))
    LddNE=ldd(nominal(9))
    LddE=ldd(nominal(6))
    LddSE=ldd(nominal(3))
    LddS=ldd(nominal(2))
    LddSW=ldd(nominal(1))
    LddW=ldd(nominal(4))
    LddNW=ldd(nominal(7))

    Elevation = PreviousHeight

    # transport to downstream neighbour
    Tmp=slopedownstream(NLdd,Elevation)*KTS
    NSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
    Tmp=slopedownstream(NELdd,Elevation)*KTS
    NESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
    Tmp=slopedownstream(ELdd,Elevation)*KTS
    ESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
    Tmp=slopedownstream(SELdd,Elevation)*KTS
    SESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
    Tmp=slopedownstream(SLdd,Elevation)*KTS
    SSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
    Tmp=slopedownstream(SWLdd,Elevation)*KTS
    SWSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
    Tmp=slopedownstream(WLdd,Elevation)*KTS
    WSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
    Tmp=slopedownstream(NWLdd,Elevation)*KTS
    NWSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
    TotSl=NSl+NESl+ESl+SESl+SSl+SWSl+WSl+NWSl

    for x in range(0, Discretisation):
       # transport to downstream neighbour
       Tmp=slopedownstream(NLdd,Elevation)*KTS
       NSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstream(NELdd,Elevation)*KTS
       NESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstream(ELdd,Elevation)*KTS
       ESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstream(SELdd,Elevation)*KTS
       SESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstream(SLdd,Elevation)*KTS
       SSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstream(SWLdd,Elevation)*KTS
       SWSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstream(WLdd,Elevation)*KTS
       WSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstream(NWLdd,Elevation)*KTS
       NWSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       TotSl=NSl+NESl+ESl+SESl+SSl+SWSl+WSl+NWSl
    
       #Elevation = cover(FixedHead,InputTS+Elevation+upstreamsperdir(NSl,NESl,ESl,SESl,SSl,SWSl,WSl,NWSl,LddN,LddNE,LddE,LddSE,LddS,LddSW,LddW,LddNW)-TotSl)
       Elevation = cover(FixedHead,InputTS+Elevation+upstreamsperdir(NSl,NESl,ESl,SESl,SSl,SWSl,WSl,NWSl,NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd)-TotSl)
       print(x)

    return Elevation



def sedflowOpti(PreviousHeight,InputCubePerYear, Diffusion, WidthStream, FixedHead, Duration, Discretisation, NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd):
    # PreviousHeight has mv elsewhere
    # Diffusion in (m3/m)/yr, volume per m width per year
    # Duration, time (yr) represented by one call
    # discretisation of Duration (nr of subtimesteps) 
    # LET OP: fixed head ook verdelen over discretisations!!
    # LET OP: NLdd, NELdd etc kan er volgens mij uit

    TimeStep=Duration/Discretisation
    # input per year (m/year)
    Input=InputCubePerYear/cellarea()
    # input per subtimestep (m/time step)
    InputTS=Input*TimeStep

    # transport (m3/year) for slope is 1
    KCuub=WidthStream*Diffusion
    # transport (m/year) for slope is 1
    KM=KCuub/cellarea()
    # transport (m/subtimestep) for slope is 1
    KTS=KM*TimeStep 

    # expected slope (-)
    SlExp=mapif(boolean(PreviousHeight),mapmaximum(cover(InputTS,scalar(-1000.0))))/KTS
    save(SlExp, "SlExp")

    AreaOfInterest=defined(PreviousHeight)
    AreaOfInterestZero=mapif(AreaOfInterest,scalar(0.0))
 
    ## create ldd's in all directions
    #LddN=mapif(AreaOfInterest,ldd(nominal(8)))
    #LddNE=mapif(AreaOfInterest,ldd(nominal(9)))
    #LddE=mapif(AreaOfInterest,ldd(nominal(6)))
    #LddSE=mapif(AreaOfInterest,ldd(nominal(3)))
    #LddS=mapif(AreaOfInterest,ldd(nominal(2)))
    #LddSW=mapif(AreaOfInterest,ldd(nominal(1)))
    #LddW=mapif(AreaOfInterest,ldd(nominal(4)))
    #LddNW=mapif(AreaOfInterest,ldd(nominal(7)))

    # modify the input ldds
    NLdd=mapif(AreaOfInterest,NLdd)
    NELdd=mapif(AreaOfInterest,NELdd)
    ELdd=mapif(AreaOfInterest,ELdd)
    SELdd=mapif(AreaOfInterest,SELdd)
    SLdd=mapif(AreaOfInterest,SLdd)
    SWLdd=mapif(AreaOfInterest,SWLdd)
    WLdd=mapif(AreaOfInterest,WLdd)
    NWLdd=mapif(AreaOfInterest,NWLdd)

    # calculate downstream distance
    NLddDownstreamDist=downstreamdist(NLdd)
    NELddDownstreamDist=downstreamdist(NELdd)
    ELddDownstreamDist=downstreamdist(ELdd)
    SELddDownstreamDist=downstreamdist(SELdd)
    SLddDownstreamDist=downstreamdist(SLdd)
    SWLddDownstreamDist=downstreamdist(SWLdd)
    WLddDownstreamDist=downstreamdist(WLdd)
    NWLddDownstreamDist=downstreamdist(NWLdd)

    Elevation = PreviousHeight

    for x in range(0, Discretisation):
       ## transport to downstream neighbour
       Tmp=slopedownstreamOptiOpti(NLdd,Elevation,AreaOfInterestZero,NLddDownstreamDist)*KTS
       NSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(NELdd,Elevation,AreaOfInterestZero,NELddDownstreamDist)*KTS
       NESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(ELdd,Elevation,AreaOfInterestZero,ELddDownstreamDist)*KTS
       ESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(SELdd,Elevation,AreaOfInterestZero,SELddDownstreamDist)*KTS
       SESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(SLdd,Elevation,AreaOfInterestZero,SLddDownstreamDist)*KTS
       SSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(SWLdd,Elevation,AreaOfInterestZero,SWLddDownstreamDist)*KTS
       SWSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(WLdd,Elevation,AreaOfInterestZero,WLddDownstreamDist)*KTS
       WSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(NWLdd,Elevation,AreaOfInterestZero,NWLddDownstreamDist)*KTS
       NWSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       TotSl=NSl+NESl+ESl+SESl+SSl+SWSl+WSl+NWSl
    
       #Elevation = cover(FixedHead,InputTS+Elevation+upstreamsperdir(NSl,NESl,ESl,SESl,SSl,SWSl,WSl,NWSl,LddN,LddNE,LddE,LddSE,LddS,LddSW,LddW,LddNW)-TotSl)
       Elevation = cover(FixedHead,InputTS+Elevation+upstreamsperdir(NSl,NESl,ESl,SESl,SSl,SWSl,WSl,NWSl,NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd)-TotSl)
       xFormat = '%3d' % (x)
       pcr.write(3 * '\b')
       pcr.write(xFormat)
       sys.stdout.flush()

    return Elevation, TotSl

def sedflowBeltsOpti(PreviousHeight,Belts,InputCubePerYear, Diffusion, WidthStream, FixedHead, Duration, Discretisation, \
    NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd):

    # this is a copy of seflowOpti, maar BeltArea (through Belts) has been added
    # BeltArea (in nr of cells) is the area of the channel belt 'connected' to each channel cell
 
    # PreviousHeight has mv elsewhere
    # Diffusion in (m3/m)/yr, volume per m width per year
    # Duration, time (yr) represented by one call
    # discretisation of Duration (nr of subtimesteps) 
    # LET OP: fixed head ook verdelen over discretisations!!
    # LET OP: NLdd, NELdd etc kan er volgens mij uit

    AreaOfInterest=defined(PreviousHeight)
    AreaOfInterestZero=mapif(AreaOfInterest,scalar(0.0))

    ChannelsId=uniqueid(AreaOfInterest)
    BeltsBooleanMV=mapif(mapne(Belts,(0)),boolean(1))
    BeltsIDs=spreadzone(nominal(ChannelsId),scalar(0),scalar(BeltsBooleanMV))
    BeltsArea=mapif(AreaOfInterest,areaarea(BeltsIDs))/cellarea()

    TimeStep=Duration/Discretisation
    # input per year (m/year)
    Input=InputCubePerYear/cellarea()
    # input per subtimestep (m/time step)
    InputTS=Input*TimeStep

    # transport (m3/year) for slope is 1
    KCuub=WidthStream*Diffusion
    # transport (m/year) for slope is 1
    KM=KCuub/cellarea()
    # transport (m/subtimestep) for slope is 1
    KTS=KM*TimeStep 

    # expected slope (-)
    SlExp=mapif(boolean(PreviousHeight),mapmaximum(cover(InputTS,scalar(-1000.0))))/KTS

 
    ## create ldd's in all directions
    #LddN=mapif(AreaOfInterest,ldd(nominal(8)))
    #LddNE=mapif(AreaOfInterest,ldd(nominal(9)))
    #LddE=mapif(AreaOfInterest,ldd(nominal(6)))
    #LddSE=mapif(AreaOfInterest,ldd(nominal(3)))
    #LddS=mapif(AreaOfInterest,ldd(nominal(2)))
    #LddSW=mapif(AreaOfInterest,ldd(nominal(1)))
    #LddW=mapif(AreaOfInterest,ldd(nominal(4)))
    #LddNW=mapif(AreaOfInterest,ldd(nominal(7)))

    # modify the input ldds
    NLdd=mapif(AreaOfInterest,NLdd)
    NELdd=mapif(AreaOfInterest,NELdd)
    ELdd=mapif(AreaOfInterest,ELdd)
    SELdd=mapif(AreaOfInterest,SELdd)
    SLdd=mapif(AreaOfInterest,SLdd)
    SWLdd=mapif(AreaOfInterest,SWLdd)
    WLdd=mapif(AreaOfInterest,WLdd)
    NWLdd=mapif(AreaOfInterest,NWLdd)

    # calculate downstream distance
    NLddDownstreamDist=downstreamdist(NLdd)
    NELddDownstreamDist=downstreamdist(NELdd)
    ELddDownstreamDist=downstreamdist(ELdd)
    SELddDownstreamDist=downstreamdist(SELdd)
    SLddDownstreamDist=downstreamdist(SLdd)
    SWLddDownstreamDist=downstreamdist(SWLdd)
    WLddDownstreamDist=downstreamdist(WLdd)
    NWLddDownstreamDist=downstreamdist(NWLdd)

    Elevation = PreviousHeight

    for x in range(0, Discretisation):
       ## transport to downstream neighbour
       Tmp=slopedownstreamOptiOpti(NLdd,Elevation,AreaOfInterestZero,NLddDownstreamDist)*KTS
       NSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(NELdd,Elevation,AreaOfInterestZero,NELddDownstreamDist)*KTS
       NESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(ELdd,Elevation,AreaOfInterestZero,ELddDownstreamDist)*KTS
       ESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(SELdd,Elevation,AreaOfInterestZero,SELddDownstreamDist)*KTS
       SESl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(SLdd,Elevation,AreaOfInterestZero,SLddDownstreamDist)*KTS
       SSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(SWLdd,Elevation,AreaOfInterestZero,SWLddDownstreamDist)*KTS
       SWSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(WLdd,Elevation,AreaOfInterestZero,WLddDownstreamDist)*KTS
       WSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       Tmp=slopedownstreamOptiOpti(NWLdd,Elevation,AreaOfInterestZero,NWLddDownstreamDist)*KTS
       NWSl=mapifelse(lt(Tmp,scalar(0.0)),scalar(0.0),Tmp)
       TotSl=NSl+NESl+ESl+SESl+SSl+SWSl+WSl+NWSl
    
       #Elevation = cover(FixedHead,InputTS+Elevation+upstreamsperdir(NSl,NESl,ESl,SESl,SSl,SWSl,WSl,NWSl,LddN,LddNE,LddE,LddSE,LddS,LddSW,LddW,LddNW)-TotSl)
       Elevation = cover(FixedHead,InputTS/BeltArea+Elevation+ \
                   upstreamsperdir(NSl,NESl,ESl,SESl,SSl,SWSl,WSl,NWSl,NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd)/BeltArea-TotSl/BeltArea)
       xFormat = '%3d' % (x)
       pcr.write(3 * '\b')
       pcr.write(xFormat)
       sys.stdout.flush()

    return Elevation, TotSl

def dispfluxOpti(Input,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal,NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd,PitAtBifurcationsLdd):
    Bifurcations=dyn.bifurcations(NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd)
    FluxDownstreamOfBifurcations=scalar(0)
    x=0
    while x < 10:
      x = x+1
      # transport over pitatbifurcationsldd
      Flux=cover(accuflux(PitAtBifurcationsLdd,Input+FluxDownstreamOfBifurcations),scalar(0))
      # distribute water over downstream neighbours at bifurcations
      FluxAtBifurcations=mapifelse(Bifurcations,Flux,scalar(0))
      FluxDownstreamOfBifurcations=upstreams(FluxAtBifurcations,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal)
    return Flux

def dispfluxperdirOpti(Input,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal,NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd,PitAtBifurcationsLdd):
    Flux = dispfluxOpti(Input,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal,NLdd,NELdd,ELdd,SELdd,SLdd,SWLdd,WLdd,NWLdd,PitAtBifurcationsLdd)
    NTotFlux,NETotFlux,ETotFlux,SETotFlux,STotFlux,SWTotFlux,WTotFlux,NWTotFlux= \
             upstreamsComponent(Flux,NScal,NEScal,EScal,SEScal,SScal,SWScal,WScal,NWScal)
    return Flux, NTotFlux, NETotFlux, ETotFlux, SETotFlux, STotFlux, SWTotFlux, WTotFlux, NWTotFlux

