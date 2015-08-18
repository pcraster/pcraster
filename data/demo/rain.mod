# model for simulation of rainfall
# 24 timesteps of 6 hours => modelling time one week

binding
 RainStations=rainstat.map;        # map with location of rainstations
 RainTimeSeries=rain.tss;          # timeseries with rain at rainstations
 RainZones=rainzone.map;           # reported stack of maps with rain
 SurfaceWater=rainfall;            # reported maps with rain (mm/6hours)

areamap
 mask.map;

timer
 1 28 1;

initial
 # coverage of meteorological stations for the whole area
 report RainZones=spreadzone(RainStations,0,1);

dynamic
 # calculate and report maps with rainfall at each timestep (mm/6 hours)
 report SurfaceWater=timeinputscalar(RainTimeSeries,RainZones);
