first: all

ROOT=$(shell nativePath.sh ${PCRTREE2}/sources/pcraster_model_engine)
Doxyfile: ${DEVENV}/Templates/sources/Doxyfile Makefile
	createdoxyfile.py $@ PROJECT_NAME="PCRaster LinkOut API" INPUT=" ${ROOT}/pcrcalc.h ${ROOT}/apiDocsMain"

all: Doxyfile
	doxygen

clean:
	rm -fr html Doxyfile

LinkOutAPIUserManual.pdf: LinkOutAPIUserManual.odt
	@echo LinkOutAPIUserManual.pdf is older then LinkOutAPIUserManual.odt
	@exit 1

