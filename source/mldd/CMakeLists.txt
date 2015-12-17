ADD_SUBDIRECTORY(python)

if(PCRASTER_BUILD_TEST)
    add_subdirectory(Tests)
endif()

SET(SOURCES
    mldd_accuflux.cc
    mldd_dagraster.cc
    mldd_diffuse.cc
    mldd_downstreamvisitor.cc
    mldd_mldd.cc
    mldd_removestream.cc
    mldd_upstream.cc
    mldd_vertexiterator.cc
    mldd_weightmap.cc
    # FILES for the oldcalc style ModelLink
    # calcLibWrapper.cc
    # mldd_mlddlink.cc
    # modellinks.cc
)


# ADD_CUSTOM_COMMAND(
#   OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml
#   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml ${CMAKE_CURRENT_SOURCE_DIR}/operation.dtd
#   COMMENT "Expanding XML to OBJECT directory"
#   COMMAND ${LIBXML2_XMLLINT_EXECUTABLE} --noout --nonet --nocatalogs --dtdvalid ${CMAKE_CURRENT_SOURCE_DIR}/operation.dtd ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml
#   COMMAND ${LIBXSLT_XSLTPROC_EXECUTABLE} --nonet --output ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml ${DEVENV}/sources/xslt/expand.xsl ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml
# )

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../api
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_model_engine
    #${CMAKE_CURRENT_BINARY_DIR} # some code creation (createOperatorCppCode.py)
)

# SET(TARGET_LINK_LIBS
#     ${PCRASTER_RASTERFORMAT_LIBRARY}
#     ${PCRASTER_DAL_LIBRARY}
#     pcrgeo
#     api
#     pcrcom
#     misc
#     mathx
#     ${QT_QTCORE_LIBRARIES}
#     ${BOOST_SYSTEM_LIBRARY}
#     ${BOOST_FILESYSTEM_LIBRARY}
# )

# ADD_LIBRARY(mldd_static
#     STATIC
#     ${SOURCES}
# )

ADD_LIBRARY(mldd
    SHARED
    ${SOURCES}
    #sourcesLibDummy.cc
)

# IF(WIN32)
#     TARGET_LINK_LIBRARIES(mldd
#         ${TARGET_LINK_LIBS}
#     )
#     # TARGET_LINK_LIBRARIES(testMldd
#     #   mldd_static
#     #   ${TARGET_LINK_LIBS}
#     #   ${QT_QTCORE_LIBRARIES}
#     #   ${BOOST_UNIT_TEST_FRAMEWORK_LIBRARY}
#     # )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(mldd
        # ${PCRASTER_RASTERFORMAT_LIBRARY}
        # ${PCRASTER_DAL_LIBRARY}
        pcrgeo
        misc
        # api
        pcrcom
        pcraster_dal
        # mathx
        # ${QT_QTCORE_LIBRARIES}
        # ${BOOST_SYSTEM_LIBRARY}
        # ${BOOST_FILESYSTEM_LIBRARY}
    )
# ENDIF(WIN32)

INSTALL(
    TARGETS mldd
    RUNTIME DESTINATION lib COMPONENT Runtime
    LIBRARY DESTINATION lib COMPONENT Runtime
    ARCHIVE DESTINATION lib COMPONENT Runtime
)
