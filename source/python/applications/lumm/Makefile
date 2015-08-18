# Makefile used only by the LUMM developers.
#
# HOWTO:
# - zet lummDir, lummInputRoot, lummOutputRoot
# - targets:
#     runcases: runt alle test cases
#     distro  : maakt zip om aan gebruiker the geven
#     docs    : genereert bron code documentatie
#     clean   : ruimt alle gegenereerde spullen weer op
#
# TODO lumm_init.R wordt toch niet meer gebruikt? weg ermee dan

system             := $(shell echo "import platform; print platform.system()" | python)
pythonModules      := binaryregression.py constants.py independentvariables.py lumm.py multinomialregression.py regression.py splitdataset.py task.py taskrunner.py utils.py
rFiles             := binaryRegression.R multinomialRegression.R
date               := $(shell date +"%y%m%d")
configurationFiles := $(shell ls cases/*.cfg)

ifeq ($(system), Linux)
  configurationFiles += $(shell ls cases/linux/*.cfg)
  lummDir          := /home/kor/pcrtree/libs/lumm
  lummInputRoot    := /home/kor/notinbackup/landusetoolkit
  lummOutputRoot   := /home/kor/tmp
  lummCommand      := python2.4 -m pdb lumm.py   # Run in debugger.
  lummCommand      := ./lumm.py
else # Windows
  lummDir          := /pcrtree/libs/lumm
  lummInputRoot    := /lumm-data
  lummOutputRoot   := /lumm-output
  lummCommand      := python lumm.py
endif

runcases:
	@set -e; for case in $(configurationFiles); \
  do \
    echo running case $$case...; \
    LUMMDIR=$(lummDir) \
    LUMMINPUTROOT=$(lummInputRoot) \
    LUMMOUTPUTROOT=$(lummOutputRoot) \
    $(lummCommand) $$case; \
  done

debug:
	LUMMDIR=$(lummDir) \
    LUMMINPUTROOT=$(lummInputRoot) \
    LUMMOUTPUTROOT=$(lummOutputRoot) \
    python2.4 -m pdb $(lummCommand) $(configurationFiles)

rt:
	pcrPython.sh test.py

distro: docs
	rm -fR lumm-$(date)
	mkdir lumm-$(date)
	cp README.TXT lumm-$(date)
	cp $(pythonModules) lumm-$(date)
	mkdir lumm-$(date)/cases
	cp -r cases/*.cfg lumm-$(date)/cases
	cp $(rFiles) lumm-$(date)
	mkdir lumm-$(date)/docs
	cp -r html lumm-$(date)/docs/source
	tar zcf lumm-$(date).tar.gz lumm-$(date)
	rm -fR lumm-$(date)

docs:
	@rm -fR html
	@epydoc --output html $(pythonModules)

clean:
	@rm -fR setup.py MANIFEST dist
	@rm -fR *.pyc
	@rm -fR html

