#! --noheader
# Rhineflow version 2.01.00
# @1999 WPA van Deursen Carthago Consultancy

binding
  prec2series	= maps\raindec.tss;	# mm/day
  precseries	= maps\rgebdec.tss;	# mm/day
  StationID	= maps\zone.map;
  meteozones	= maps\zone.map;
  rainzones	= maps\chrarea.sml;

  AfvoerID	= maps\qstat.sml;
  MinTSeries	= maps\sltmin.tss;
  MaxTSeries	= maps\sltmax.tss;
  MeanTSeries	= maps\sltave.tss;
  EREFSeries	= maps\etpdec.tss;
  DecLenSeries	= maps\rf2dec.tss;

  dem		= maps\dem.sml;
  RiverLDD	= maps\lddr1.sml;

  CF		= maps\cropf.sml;
  whold		= maps\whold.sml;
  # rc  = recession constant
  rc		= maps\rc.sml;
  
  SubCatchment  = maps\subcatch.sml;

  ereft		= maps\ereft.lut;


#  Precipitation	= Prec;
#  Snowcover	= SC;
#  m3out		= m3out.tss;

  # SFT = snowfall trigger temperature
  SFT 		= 0.817;
  # SMT = snowmelt trigger temperature
  SMT 		= 0.817;
  # SMR = snowmelt rate
  SMR 		= 1;

  # sep = separation constant
  DirectRunoffCoeff = 0.1;
  SoilStorZero	= 0.001;

  # Accu Fraction glacier
  AFG		=	0.1;	
  GlacierThreshold=	1000;

  dtseries	= dtdec.tss;
  dtid    	= dtid.map;
  dpseries	= dpdec.tss;
  dpid    	= dpid.map;

timer 1 1260 1;


initial
	CropFactor	= CF*1.6;
	CropFactor	= if(SubCatchment eq 7,CF*0.8,CropFactor);
	CropFactor	= if(SubCatchment eq 4,CF*1.4,CropFactor);
	sep		= if(SubCatchment eq 7,0.1, scalar(0.2));
	rc		= rc*25;
	whold		= max(whold,SoilStorZero);
	Snowcover	= 0*dem;
	Glacier		= scalar(0);

  # SoilStor (mm)
	SoilStor	= whold;
  # gwStor (mm)
	gwStor		= 100;
  # sloFlo (mm/day)
	sloFlo		= 0;

	ups		= accuflux(RiverLDD,scalar(1));
	h2t		= dem*0.006;
dynamic
  # Inlezen temp en prec variabelen
  # Precipitation = Neerslag (mm/day)
  # MinTemp = minimum temperature timestep
  # MaxTemp = maximum temperature timestep


	Maand		= timeinputscalar(DecLenSeries,2);
	DecLength	= timeinputscalar(DecLenSeries,4);
	Cycle		= timeinputscalar(DecLenSeries,5);

  # temperature increase (C)
  dtv		= timeinputscalar(dtseries,dtid);
  # precipitation change (%)
  dpv		= timeinputscalar(dpseries,dpid);

	Precipitation	= cover(timeinputscalar(precseries,rainzones),timeinputscalar(prec2series,meteozones));

	MinTemp		= timeinputscalar(MinTSeries,StationID)-h2t;
	MaxTemp		= timeinputscalar(MaxTSeries,StationID)-h2t;
	MeanTemp	= timeinputscalar(MeanTSeries,StationID)-h2t;
	ERef		= max(0.01,timeinputscalar(EREFSeries,StationID));

 MinTemp	= MinTemp + dtv;
 MaxTemp	= MaxTemp + dtv;
 MeanTemp	= MeanTemp + dtv;
 ERef		= ERef + dtv*lookupscalar(ereft,Maand);
 Precipitation	= Precipitation*(100+dpv)/100;

# report pre.tss = timeoutput(AfvoerID,catchmenttotal(Precipitation,RiverLDD)/ups);
# report temp.tss= timeoutput(AfvoerID,catchmenttotal(MeanTemp,RiverLDD)/ups);

# Reference crop evaporation (mm/day)
report	ERef.tss	= timeoutput(AfvoerID,accuflux(RiverLDD,ERef)/ups);

	Glacier		= accufractionstate(RiverLDD,Glacier,AFG);

	SFF		= max(0,min(1,(SFT-MinTemp)/(MaxTemp-MinTemp)));
  # Snowfall (mm/day)
	Snowfall	= SFF*Precipitation;
  # Rainfall (mm/day)
	Rainfall	= Precipitation - Snowfall;
  # Snowcover (mm)
	Snowcover	= Snowcover+Snowfall*DecLength;

	SMF		= max(0,min(1,(MaxTemp-SMT)/(MaxTemp-MinTemp)));
  # Snowmelt (mm/day)
	Snowmelt	= min(Snowcover,SMF*SMR*DecLength*(MaxTemp-MinTemp)/2)/DecLength;
	Snowcover	= Snowcover-Snowmelt*DecLength;
  # snow cover (mm)	
report	sc.tss		= timeoutput(AfvoerID,accuflux(RiverLDD,Snowcover)/ups);

	Glacier		= max(Snowcover-GlacierThreshold,0);
	Snowcover	= Snowcover - Glacier;

	SCrop		= Cycle*(CropFactor-1)+1;
  # peCrop (mm/day)
	peCrop		= SCrop*ERef;

  # Overland flow (mm/day)
	OverlandFlow	= (Rainfall+Snowmelt)*DirectRunoffCoeff;

  # PEff (mm/day)
	PEff		= Rainfall+Snowmelt-OverlandFlow-peCrop;
	nsmd		= min(0,-whold*ln(whold/SoilStor)+PEff);
	dst		= (whold*exp(nsmd/whold)-SoilStor)/(30/DecLength);

  # AE (mm/day)
	AE		= min(peCrop,Rainfall+Snowmelt-OverlandFlow-dst);
report	ae.tss		= timeoutput(AfvoerID,accuflux(RiverLDD,AE)/ups);


	SoilStor	= max(SoilStorZero,SoilStor+(Rainfall+Snowmelt-OverlandFlow-AE)*DecLength);
  # Excess (mm/day)
	Excess		= max(0,SoilStor-whold)/DecLength;
	SoilStor	= SoilStor-Excess*DecLength;

  # kwikFlo (mm/day)
	kwikFlo		= sep*Excess;
	Excess		= Excess-kwikFlo;
 
	gwStor		= gwStor+(Excess-sloFlo)*DecLength;
	gwStor		= max(0,gwStor);
	sloFlo		= gwStor/rc;

  # runoff a.k.a qout and discharge  (mm/day) (eventually m3/sec)
	runoff		= kwikFlo + sloFlo + OverlandFlow;
report	discharge.tss = timeoutput(AfvoerID,accuflux(RiverLDD,runoff)/ups);

# in addition dpv and dpt are reported seperate
