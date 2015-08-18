LDD=Eout.map + NEout.map  + Nout.map + NWout.map  + SEout.map  + Sout.map + SWout.map  + Wout.map
W= E_w.map + NE_w.map  + N_w.map + NW_w.map  + SE_w.map  + S_w.map + SW_w.map  + W_w.map

all:
	runapp pcrcalc -f test.pcr
	runapp pcrcalc -f diffuse.pcr
	runapp aguila $(W) + $(LDD)
	runapp aguila upstream.map + $(LDD)
	runapp aguila accuflux.map + $(LDD)
