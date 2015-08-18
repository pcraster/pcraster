binding
	
	#динамический вход
	precseries	= prec_dec.tss;	# mm/day	#осадки за 10 суток
	meanTempseries	= taver_dec.tss;		#средняя среднесуточная температура воздуха за 10 суток
	maxTempseries	= tmax_dec.tss;		#максимальная среднесуточная температура воздуха за 10 суток
	minTempseries	= tmin_dec.tss;		#минимальная среднесуточная температура на поверхности почвы за 10 суток
	stationpoly	= friction.map;		#карта тиссеновских полигонов метеостанций
	stationpoints	= station.map;		#карта метеостанций
	
	#остальные данные
	dem		= latest_dtm.map;			#цифровая модель рельефа
	riverldd	= ldd_catchment.ldd;		#тальвежно-русловая сеть территории
	whold		= whold_cover.map;		#карта полевой влагоемкости почвы
	cropfactor	= cropf_cover.map;		#карта распределения коэффициента коррекции на испарение растительностью	
	observationpoint	=q_obs.map;			#замыкающий створ
	thorni	= thorn_i.lut;			#таблица параметра I в формуле Торнтуэйта для каждой метеостанции
	thorna	= thorn_a.lut;			#таблица параметра а в формуле Торнтуэйта для каждой метеостанции
	nrdaystss	=nrdays10.tss;
	daylenghtss	=daylen10.tss;
	snowfalltriggertemp	=0.7;			#температура начала снегопада
	snowmeltrate	=30;	#mm/deg*tstep	#интенсивность снеготаяния
	daypertimestep	=10;				#количество дней за 1 шаг
	snowmelttemp	=4;				#температура начала снеготаяния
	soilfreezetemp	=-1;				#температура начала промерзания почвы
	soilthawtemp	=2;				#температура начала оттаивания
	separationconst	=0.6;				#коэффициент просачивания                                                             	recession	=0.01;				#коэффициент базисного стока
					
	
	
timer 
1	36	1;
initial
#начальные условия 	
	soilstorage	=0;	#влагозапас в почве (мм)
	snowcover 	=0;	#высота снежного покрова (мм)
	soilfrozen 	=0;	#состояние почвы (1 - замерзшая, 0 - незамерзшая)
	basereservoir	=200;	#емкость базисного стока
	upstreamarea	=accuflux(riverldd,1); #площадь водосбора до замыкающего створа
	dp=1.2;	#изменение количества осадков
	dt=2;		#изменение температуры
dynamic
	#	распределение осадков по территории методом обратной дистанции
	precipitationpoints=timeinputscalar(precseries,stationpoints)*daypertimestep;	#mm/tstep
	precipitation=inversedistance(defined(dem),precipitationpoints,2,0,0);

	#	распределение температуры по территории методом обратной дистанции
	meanTemppoints=timeinputscalar(meanTempseries,stationpoints);
	meanTemp=inversedistance(defined(dem),meanTemppoints,2,0,0);
	minTemppoints=timeinputscalar(minTempseries,stationpoints);
	minTemp=inversedistance(defined(dem),minTemppoints,2,0,0);
	maxTemppoints=timeinputscalar(maxTempseries,stationpoints);
	maxTemp=inversedistance(defined(dem),maxTemppoints,2,0,0);
	
	#	выпадение жидких и твердых осадков, снеготаяние и высота снежного покрова
	snowfall=if(meanTemp lt snowfalltriggertemp then precipitation else 0);	#mm/tstep
	snowcover=snowfall+snowcover;		#mm
	snowmelt=min(snowcover, if(meanTemp gt snowmelttemp then snowmeltrate*meanTemp else 0));	#mm/tstep
	snowcover=snowcover-snowmelt;
	rainfall=precipitation-snowfall;	#mm/tstep
	
	#	промерзание почвы
	soilfreeze=minTemp lt soilfreezetemp;
	soilthaw=(snowcover eq 0) and (meanTemp gt soilthawtemp);
	soilfrozen=(soilfrozen or soilfreeze) and not (soilthaw);
	
	#	испаряемость по Торнтуэйту
	heatindex=lookupscalar(thorni,stationpoly);
	aindex=lookupscalar(thorna,stationpoly);
	tmp=cover((10*meanTemp/heatindex)**aindex,0);
	potevap=if((meanTemp gt 0) and (heatindex ge 0) then 16*tmp*timeinputscalar(daylenghtss,stationpoly) else 0);
	potevapcorr=0.1*(timeinputscalar(nrdaystss,1)/30);
	pottotal=potevap*cropfactor;
	evaporation=min(pottotal,soilstorage);	
	soilstorage=soilstorage-evaporation;	

	#	поверхностный сток и инфильтрация
	surfacewater=rainfall+snowmelt;	#mm/tstep	
	filtration=if(soilfrozen then 0 else min(surfacewater,100));	#mm/tstep
	runoff=surfacewater-filtration;		#mm/tstep
	soilstorage=soilstorage+filtration;
	excess=max(0,soilstorage-whold);	#mm/tstep
	soilstorage=soilstorage-excess;
	percolation=excess*separationconst;	#mm/tstep

	
	#	базисный сток
	basereservoir=basereservoir+percolation;	#mm
	baseflow=basereservoir*recession;	#mm/tstep
	basereservoir=basereservoir-baseflow;	#mm

	#	суммарный сток
	quickflow=runoff+excess-percolation;	#mm/tstep
	discharge=quickflow+baseflow;	#mm/tstep
	accudis=accuflux(riverldd,discharge)/upstreamarea;

	#	выходные данные
	#report	okadischarge.tss=timeoutput(observationpoint,accudis); #mm/tstep
	#report filt.tss=timeoutput(1,filtration);
	report okasoils.tss=timeoutput(1,soilstorage);
	#report perc.tss=timeoutput(1,percolation);
	#report basef.tss=timeoutput(1,baseflow);
	#report surf.tss=timeoutput(1,surfacewater);
	#report pot.tss=timeoutput(1,potevaporation);
	report okaevap.tss=timeoutput(1,pottotal);
	#report okaexcess.tss=timeoutput(1,excess);
	#report base.tss=timeoutput(1,basereservoir);
	report okaquick.tss=timeoutput(1,quickflow);
	report okabase.tss=timeoutput(1,baseflow);
	report okarun.tss=timeoutput(1,runoff);



