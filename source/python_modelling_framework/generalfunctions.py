#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os

import pcraster as pcr

# time in hours


def getCellValue(Map, Row, Column):
    Value, Valid = pcr.cellvalue(Map, Row, Column)
    if Valid:
        return Value
    else:
        print('missing value in input of getCellValue')


def getCellValueAtBooleanLocation(location, map):
    # map can be any type, return value always float
    valueMap = pcr.mapmaximum(pcr.ifthen(location, pcr.scalar(map)))
    value = getCellValue(valueMap, 1, 1)
    return value


def printCellValue(self, mapVariable, variableNameToPrint, unit, row, column):
    cellValue = getCellValue(mapVariable, row, column)
    print(variableNameToPrint + ' (' + unit + ') at row ' + str(row) + ', column: ' + str(column) + ' is: ' + str(cellValue))


def onePeriod(self, startTime, endTime, timeStepDuration, currentTimeStep):
    # this could be separated in two functions, one converting hours to
    # time steps, one creating the period
    time = float(currentTimeStep) * float(timeStepDuration)
    period = (time > startTime) & (time < endTime)
    return period


def mapeq(mapOne, mapTwo):
    mapOneScalar = pcr.scalar(mapOne)
    mapTwoScalar = pcr.scalar(mapTwo)
    difference = mapOneScalar - mapTwoScalar
    cellEqual = pcr.pcreq(difference, pcr.scalar(0))
    mapEqual = pcr.pcrgt(pcr.mapminimum(pcr.scalar(cellEqual)), pcr.scalar(0.5))
    return getCellValue(mapEqual, 1, 1)


def slopeToDownstreamNeighbour(dem, ldd):
    slopeToDownstreamNeighbour = (dem - pcr.downstream(ldd, dem)) / pcr.downstreamdist(ldd)
    return slopeToDownstreamNeighbour


def slopeToDownstreamNeighbourNotFlat(dem, ldd, minSlope):
    slopeToDownstreamNeighbourMap = slopeToDownstreamNeighbour(dem, ldd)
    lddArea = pcr.defined(ldd)
    minSlopeCover = pcr.ifthen(lddArea, pcr.scalar(minSlope))
    slopeToDownstreamNeighbourNotFlat = pcr.cover(pcr.max(minSlopeCover, slopeToDownstreamNeighbourMap), minSlopeCover)
    return slopeToDownstreamNeighbourNotFlat


def distancetodownstreamcell(Ldd):
    distanceToDownstreamCell = pcr.max(pcr.downstreamdist(Ldd), pcr.celllength())
    return distanceToDownstreamCell


def createTimeSeriesList(timeSeriesFile):
    file = open(timeSeriesFile, 'r')
    piet = file.readlines()
    newList = []
    for line in piet:
        lineList = string.split(line)
        newList.append(lineList)
    file.close()
    return newList


def timeInputSparse(fileName):
    return os.path.exists(fileName)


def normalcorrelated(normalX, normalY, correlation):
    # returns realizations of two normal variables with
    # mean zero and var 1 having correlation of correlation
    # based on:
    # x=normal()
    # y=ax+b*normal()
    # correlation = a /  sqrt( sqr(a) + sqr(b) )
    x = pcr.scalar(normalX)
    y = (x + pcr.sqrt((1 / pcr.sqr(correlation)) - 1) * pcr.scalar(normalY)) * pcr.scalar(correlation)
    return x, y
