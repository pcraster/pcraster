add_subdirectory(python)

if(PCRASTER_BUILD_TEST)
    add_subdirectory(Tests)
endif()

# ADD_CUSTOM_COMMAND(
#   OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml
#   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml ${CMAKE_CURRENT_SOURCE_DIR}/operation.dtd
#   COMMENT "Expanding XML to OBJECT directory"
#   COMMAND ${LIBXML2_XMLLINT_EXECUTABLE} --noout --nonet --nocatalogs --dtdvalid ${CMAKE_CURRENT_SOURCE_DIR}/operation.dtd ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml
#   COMMAND ${LIBXSLT_XSLTPROC_EXECUTABLE} --nonet --output ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml ${DEVENV}/sources/xslt/expand.xsl ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml
# )

add_library(mldd STATIC
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
    # calcLibWrapper
    # mldd_mlddlink
    # modellinks

    # sourcesLibDummy
)

add_library(pcraster::mldd ALIAS mldd)

target_include_directories(mldd
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(mldd
    PUBLIC
        pcraster::pcrgeo
        Boost::headers
)

# install(
#     TARGETS
#         mldd
#     RUNTIME
#         DESTINATION lib COMPONENT Runtime
#     LIBRARY
#         DESTINATION lib COMPONENT Runtime
#     ARCHIVE
#         DESTINATION lib COMPONENT Runtime
# )
