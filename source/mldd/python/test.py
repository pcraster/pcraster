# Test script to check everything works.
import os
import sys

sys.path.append(os.environ["PCRTREE2"] + "/bin")

import pcraster
import pcraster.mldd

dataPath = "../demo"

pcraster.setclone(os.path.join(dataPath, "ELddF000.out"))

mldd = pcraster.mldd.initialise(pcraster.clone())

mldd.setStream(
         # TODO Create overload which takes strings, or handle internally.
         pcraster.readmap(os.path.join(dataPath, "ELddF000.out")),
         pcraster.readmap(os.path.join(dataPath, "NELddF00.out")),
         pcraster.readmap(os.path.join(dataPath, "NLddF000.out")),
         pcraster.readmap(os.path.join(dataPath, "NWLddF00.out")),
         pcraster.readmap(os.path.join(dataPath, "SELddF00.out")),
         pcraster.readmap(os.path.join(dataPath, "SLddF000.out")),
         pcraster.readmap(os.path.join(dataPath, "SWLddF00.out")),
         pcraster.readmap(os.path.join(dataPath, "WLddF000.out")))
mldd.addStream(
         pcraster.readmap(os.path.join(dataPath, "ELddF000.out")))
mldd.setDem(pcraster.spatial(pcraster.scalar(1)))


upstream = mldd.upstream(pcraster.spatial(pcraster.scalar(1)))
pcraster.report(upstream, "upstream.map")

accuflux = mldd.accuflux(pcraster.ifthen(pcraster.defined(upstream),
         pcraster.spatial(pcraster.scalar(1))))
pcraster.report(accuflux, "accuflux.map")

dem = mldd.getDem()
pcraster.report(dem, "dem.map")

streamN, streamNE, streamE, streamSE, streamS, streamSW, streamW, streamNW = \
         mldd.getStream()

pcraster.report(streamN , "streamN.map")
pcraster.report(streamNE, "streamNE.map")
pcraster.report(streamE , "streamE.map")
pcraster.report(streamSE, "streamSE.map")
pcraster.report(streamS , "streamS.map")
pcraster.report(streamSW, "streamSW.map")
pcraster.report(streamW , "streamW.map")
pcraster.report(streamNW, "streamNW.map")

weightN, weightNE, weightE, weightSE, weightS, weightSW, weightW, weightNW = \
         mldd.getWeight()

pcraster.report(weightN , "weightN.map")
pcraster.report(weightNE, "weightNE.map")
pcraster.report(weightE , "weightE.map")
pcraster.report(weightSE, "weightSE.map")
pcraster.report(weightS , "weightS.map")
pcraster.report(weightSW, "weightSW.map")
pcraster.report(weightW , "weightW.map")
pcraster.report(weightNW, "weightNW.map")

print("Ok")
