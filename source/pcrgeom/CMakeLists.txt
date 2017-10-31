include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
)

SET(SOURCES
    geom_line.cc
    geom_point.cc
    geom_util.cc
)
ADD_LIBRARY(pcrgeom
    ${SOURCES}
)
