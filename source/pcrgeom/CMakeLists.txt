include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
)

SET(SOURCES
    geom_line.cc
    geom_point.cc
    #geom_prdimtree.cc
    #geom_prdimtreestats.cc
    geom_proximitysearch.cc
    geom_util.cc
)
#SET(TEST_SOURCES
#  geom_prdimtreetest.cc
#  geom_test.cc
#)
ADD_LIBRARY(pcrgeom
    ${SOURCES}
)
#ADD_EXECUTABLE(testpcrgeom
#  ${TEST_SOURCES}
#)
#TARGET_LINK_LIBRARIES(testpcrgeom
#  pcrgeom
#  ${BOOST_UNIT_TEST_FRAMEWORK_LIBRARY}
#)
# pcrLibs = ['pcrgeo', 'pcrcom'
