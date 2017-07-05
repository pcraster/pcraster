import platform
# TODO, no examples are in ../apps/resample.xml
# sample = newSample("resample", "Result1")
# sample.appendInput("Clone1")
# sample.appendInput("Map1", ext="txt")
# sample.setClone(0)
# sample.code = Cmd("-S -m mv -v 4 $ColFile1 $Result1")
# #
# # # missing CLone1
# # resample.Result1.omap: resample.Clone1.imap resample.Map1.imap
# # 	$(resample)  resample.Clone1.imap resample.Map1.imap resample.Result2.omap
# #
# # resample.Result2.omap: resample.Map12.imap resample.Map22.imap
# # 	$(resample) -b 0 resample.Map12.imap resample.Map22.imap resample.Result2.omap
# #
# # resample.Result3.omap: resample.Map12.imap
# # 	$(resample)  -r 1 -e resample.Map12.imap resample.Result3.omap


sample = newSample("asc2map", "Result1")
sample.appendInput("mapclone", useFrom="mapattr")
sample.appendInput("AscFile1", ext="txt")
sample.setClone(0)
sample.code = Cmd("-S -m MV $AscFile1 $Result1")

sample = newSample("asc2map", "Result2")
sample.appendInput("mapclone", useFrom="mapattr")
sample.appendInput("AscFile2", ext="txt")
sample.setClone(0)
sample.code = Cmd("--degrees -D -a $AscFile2 $Result2")

sample = newSample("col2map", "Result1")
sample.appendInput("mapclone", useFrom="mapattr")
sample.appendInput("ColFile1", ext="txt")
sample.setClone(0)
sample.code = Cmd("-S -m mv -v 4 $ColFile1 $Result1")

# testres is gelijk aan col2map maar met data, dat maakt echter geen zak uit
# slaat dit op hier boven of hier onder?

sample = newSample("col2map", "Result2")
sample.appendInput("mapclone", useFrom="mapattr")
sample.appendInput("ColFile2", ext="txt", autoCreatedInput=True)
sample.setClone(0)
sample.code = Cmd("-O -m mv -x 2 -y 3 -v 6 --coorlr -H $ColFile2 $Result2")

sample = newSample("map2asc", "AscFile1", "txt")
sample.appendInput("PCRmap")
sample.code = Cmd("-m m $PCRmap $AscFile1")

sample = newSample("map2asc", "AscFile2", "txt")
sample.appendInput("PCRmap")
sample.code = Cmd("-m m -s s -f 3.1f $PCRmap $AscFile2")

sample = newSample("map2asc", "AscFile3", "txt")
sample.appendInput("PCRmap")
sample.code = Cmd("-m m -a $PCRmap $AscFile3")

sample = newSample("map2col", "ColFile1", "txt")
sample.appendInput("PCRmap", useFrom="map2asc")
sample.globalOptions+=["coorul"]
sample.code = Cmd("$PCRmap $ColFile1")


# old comment here said that Dal uses this for a test
sample = newSample("map2col", "ColFile2", "txt")
sample.appendInput("PCRmap", useFrom="map2asc")
sample.globalOptions+=["coorcentre"]
sample.code = Cmd("-M -m -9999 -f 5.0f -g -c $PCRmap $ColFile2")
# -g geoeas puts filename with prefix in file (use sed to fix it)
if platform.system() == "Windows":
    sample.execPostProcessing="more + 1 $ColFile2 > $ColFile2"
else:
    sample.execPostProcessing="sed -i 's/map2asc.//g' $ColFile2"

# OLS 06/2017: this one is not used at all?
# sample = newSample("map2col", "ColFile3", "txt")
# sample.appendInput("ColFile2", ext="txt", autoCreatedInput=True)
# sample.appendInput("PCRmap3")
# sample.globalOptions+=["coorcentre"]
# sample.code = Cmd("-a $ColFile2 $PCRmap3 $ColFile3")
# # append to a geoeas file puts filename in file (use sed to fix it)
# sample.execPostProcessing="sed -i 's/map2asc.PCRmap3/PCRmap3p/g' $ColFile3"

# TODO add map2col sample with multiple input maps

sample = newSample("table", "Result1", "txt")
sample.appendInput("PCRmap1")
sample.code = Cmd("-n 4 -0 $PCRmap1 $Result1")

sample = newSample("table", "Result2", "txt")
sample.appendInput("PCRmap1")
sample.code = Cmd("-n 4 -h -0 $PCRmap1 $Result2")

sample = newSample("table", "Result3", "txt")
sample.appendInput("PCRmap1")
sample.appendInput("PCRmap2")
sample.appendInput("Input", ext="txt")
sample.code = Cmd("-i $Input $PCRmap1 $PCRmap2 $Result3")

sample = newSample("table", "Result4", "txt")
sample.appendInput("Input2", ext="txt")
sample.code = Cmd("-m 2 -i $Input2 $Result4")


# TODO
#  use of these maps in manual should
#  list loc. attrs. not the map contents
sample = newSample("mapattr", "mapclone")
sample.code = Cmd("-s -R3 -C4 -l10 -B -x20 -y50 $mapclone")


# clones in this section are exclusively in the .itxt files used
# # AscHeader2map will call make (this Makefile) to create if needed
sample = newSample("mapattr", "resample2vier")
sample.code = Cmd("-s -R3 -C4 -l2 -B -x0 -y6 $resample2vier")

sample = newSample("mapattr", "vierresample")
sample.code = Cmd("-s -R4 -C4 -l2 -B -x4 -y4 -a45 $vierresample")


sample = newSample("mapattr", "vier")
sample.code = Cmd("-s -R4 -C4 -l2 -B -x0 -y0 $vier")

sample = newSample("mapattr", "resample2zes")
sample.code = Cmd("-s -R5 -C6 -l1 -B -x5 -y7 $resample2zes")

sample = newSample("mapattr", "zes")
sample.code = Cmd("-s -R6 -C6 -l1 -B -x0 -y0 $zes")

sample = newSample("mapattr", "2vijf")
sample.code = Cmd("-s -R5 -C5 -l2 -B -x0 -y0 $2vijf")

sample = newSample("mapattr", "50vijf")
sample.code = Cmd("-s -R5 -C5 -l50 -B -x0 -y0 $50vijf")

sample = newSample("mapattr", "5xoyo1020vijf")
sample.code = Cmd("-s -R5 -C5 -l5 -B -x10 -y20 $5xoyo1020vijf")

sample = newSample("mapattr", "vijf")
sample.code = Cmd("-s -R5 -C5 -l1 -B -x0 -y0 $vijf")

sample = newSample("mapattr", "4bdrie")
sample.code = Cmd("-s -R3 -C3 -l4 -B -x6 -y6  $4bdrie")

sample = newSample("mapattr", "4drie")
sample.code = Cmd("-s -R3 -C3 -l4 -B -x10 -y10 $4drie")

sample = newSample("mapattr", "drie")
sample.code = Cmd("-s -R3 -C3 -l1 -B -x0 -y0 $drie")

sample = newSample("mapattr", "ttclone")
sample.code = Cmd("-s -R7 -C3 -l10 -B -x0 -y0 $ttclone")

sample = newSample("mapattr", "musclone")
sample.code = Cmd("-s -R2 -C1 -l10 -B -x0 -y20 $musclone")

sample = newSample("mapattr", "mustclone")
sample.code = Cmd("-s -R2 -C2 -l500 -B -x0 -y1000 $mustclone")

sample = newSample("mapattr", "attfTests.clone1")
sample.code = Cmd("-s -R1 -C5 -l1 -B -x0 -y0 $attfTests.clone1")

sample = newSample("mapattr", "attfTests.clone2")
sample.code = Cmd("-s -R1 -C6 -l1 -B -x0 -y1 $attfTests.clone2")

sample = newSample("mapattr", "attfTests.clone3")
sample.code = Cmd("-s -R2 -C5 -l1 -B -x1 -y2 $attfTests.clone3")


# end section for clone used in 'itxt's files
