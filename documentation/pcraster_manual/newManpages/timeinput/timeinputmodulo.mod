#example of timeinputmodulo workings

binding

areamap
 rain0000.002;

timer
 1 7 1;

initial

dynamic

#only rain0000.001+002 (all 1s) and rain0000.006 (all 2s) are available

 report rainmodu = timeinputmodulo(rain,3);

#result is rainFall.001 to rainFall.005 are 1s, rainfall.006 and 007 are 2s
