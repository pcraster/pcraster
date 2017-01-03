# Test script to check everything works.
import os
import sys

import pcraster
import pcraster.moc

dataPath = "../../Mldd/demo"

pcraster.setclone(os.path.join(dataPath, "DemIn"))

timeIncrement = 10
nrParticles = 5
raster = pcraster.readmap(os.path.join(dataPath, "DemIn"))
initialConcentration = (raster / raster) + 2.5
effectivePorosity = raster
storageCoefficient = raster

moc = pcraster.moc.initialise(pcraster.clone(), timeIncrement, nrParticles,
         initialConcentration, effectivePorosity, storageCoefficient)

flux = raster
flowX = raster
flowY = raster
longitudinalDispersionCoefficient = raster
transverseDispersionCoefficient = raster
hydraulicHead = raster
saturatedThickness = raster

concentration, particlesPerCell = moc.transport(flux, flowX, flowY,
          longitudinalDispersionCoefficient, transverseDispersionCoefficient,
          hydraulicHead, saturatedThickness)

changeInConcentration = (raster / raster) + 1.5
concentration = moc.adjust(changeInConcentration)

print(type(concentration))

pcraster.report(concentration, "concentration.map")

print("Ok")

