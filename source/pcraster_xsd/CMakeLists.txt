# # <bad cmake practice>
# IF(NOT "$ENV{AGUILA}" STREQUAL "")
#     SET(AGUILA_BUILD 1)
# ENDIF()
# 
# IF(NOT "$ENV{PCRTREE2}" STREQUAL "")
#     SET(PCRTREE2_BUILD 1)
# ENDIF()
# </bad cmake practice>
SET(AGUILA_BUILD 1)
SET(PCRTREE2_BUILD 1)

include_directories(
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ../utils
)

# ------------------------------------------------------------------------------
MACRO(TEXT_TO_CHARACTER_ARRAY TEXT_FILE IDENTIFIER)
    ADD_CUSTOM_COMMAND(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${IDENTIFIER}.h
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS ${TEXT_FILE}
        COMMENT Generating ${IDENTIFIER}.h
        COMMAND ${PYTHON_EXECUTABLE}
            ${CMAKE_CURRENT_SOURCE_DIR}/text2CharArray.py ${TEXT_FILE}
            ${IDENTIFIER}
    )
ENDMACRO(TEXT_TO_CHARACTER_ARRAY)

SET(XSD_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../xmlschema)
SET(XSD_EPILOGUE ${CMAKE_CURRENT_SOURCE_DIR}/xsdEpilogue.txt)
SET(XSD_OPTIONS cxx-tree
    # --std c++11
    --generate-serialization
    --generate-ostream
    --hxx-suffix XSD.h
    --cxx-suffix XSD.cc
)
# ------------------------------------------------------------------------------
SET(SOURCES
    pcrxsd_dominput.cc
    pcrxsd_StringDomInput.cc
    pcrxsd_library.cc
    pcrxsd_supportedschema.cc
    pcrxsd_utils.cc
    ${CMAKE_CURRENT_BINARY_DIR}/compiled_commonTypes.h
    ${CMAKE_CURRENT_BINARY_DIR}/commonTypesXSD.cc
)
SET(_SUPPORTED_SCHEMAS
    ${CMAKE_CURRENT_BINARY_DIR}/compiled_commonTypes.h
)
TEXT_TO_CHARACTER_ARRAY(
    ${XSD_SRC_DIR}/commonTypes.xsd
    compiled_commonTypes
)

ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/commonTypesXSD.cc
    DEPENDS ${XSD_SRC_DIR}/commonTypes.xsd ${XSD_EPILOGUE}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT Generating ${CMAKE_CURRENT_BINARY_DIR}/commonTypesXSD.cc
    COMMAND ${XSD_EXECUTABLE} ${XSD_OPTIONS}
        --hxx-epilogue "#include \"commonTypesUtilEpilogue.h\""
        --cxx-epilogue "#include \"commonTypesUtilEpilogueImpl.h\""
        ${XSD_SRC_DIR}/commonTypes.xsd
    VERBATIM
)

IF(${AGUILA_BUILD})
    SET(SOURCES
        ${SOURCES}
        ${CMAKE_CURRENT_BINARY_DIR}/compiled_Aguila.h
        ${CMAKE_CURRENT_BINARY_DIR}/AguilaXSD.cc
    )
    SET(_SUPPORTED_SCHEMAS
        ${_SUPPORTED_SCHEMAS}
        ${CMAKE_CURRENT_BINARY_DIR}/compiled_Aguila.h
    )
    SET(_COMPILE_FLAGS "${_COMPILE_FLAGS} -DAGUILA_BUILD")
    SET(AGUILA_RES
        --root-element aguila --root-element aguilaCursor
        --root-element aguilaCursorValues)

    set(_AGUILA_XSD ${XSD_SRC_DIR}/Aguila.xsd)

    ADD_CUSTOM_COMMAND(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/AguilaXSD.cc
        DEPENDS ${_AGUILA_XSD} ${XSD_EPILOGUE}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT Generating ${CMAKE_CURRENT_BINARY_DIR}/AguilaXSD.cc
        COMMAND cmake -E copy ${_AGUILA_XSD} ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND cmake -E copy ${XSD_SRC_DIR}/commonTypes.xsd
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND cmake -E copy ${CMAKE_CURRENT_SOURCE_DIR}/xsdEpilogue.txt
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${XSD_EXECUTABLE} ${XSD_OPTIONS} ${AGUILA_RES}
            --hxx-epilogue-file xsdEpilogue.txt Aguila.xsd
    )

    TEXT_TO_CHARACTER_ARRAY(
        ${_AGUILA_XSD}
        compiled_Aguila
    )
ENDIF()

IF(${PCRTREE2_BUILD})
    SET(SOURCES
        ${SOURCES}
        ${CMAKE_CURRENT_BINARY_DIR}/compiled_PCRaster.h
        ${CMAKE_CURRENT_BINARY_DIR}/PCRasterXSD.cc
    )
    SET(_SUPPORTED_SCHEMAS
        ${_SUPPORTED_SCHEMAS}
        ${CMAKE_CURRENT_BINARY_DIR}/compiled_PCRaster.h
    )
    SET(_COMPILE_FLAGS "${_COMPILE_FLAGS} -DPCRTREE2_BUILD")
    set(_PCRASTER_XSD ${XSD_SRC_DIR}/PCRaster.xsd)
    ADD_CUSTOM_COMMAND(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/PCRasterXSD.cc
        DEPENDS ${_PCRASTER_XSD} ${XSD_EPILOGUE}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT Generating ${CMAKE_CURRENT_BINARY_DIR}/PCRasterXSD.cc
        COMMAND cmake -E copy ${_PCRASTER_XSD} ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND cmake -E copy ${XSD_SRC_DIR}/commonTypes.xsd
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND cmake -E copy ${CMAKE_CURRENT_SOURCE_DIR}/xsdEpilogue.txt
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${XSD_EXECUTABLE} ${XSD_OPTIONS} --root-element-all
            --cxx-suffix XSD.cc --hxx-epilogue-file xsdEpilogue.txt PCRaster.xsd
    )

    TEXT_TO_CHARACTER_ARRAY(
        ${_PCRASTER_XSD}
        compiled_PCRaster
    )
ENDIF()

SET_SOURCE_FILES_PROPERTIES(pcrxsd_supportedschema.cc
    PROPERTIES
    OBJECT_DEPENDS "${_SUPPORTED_SCHEMAS}"
    COMPILE_FLAGS "${_COMPILE_FLAGS}"
)

ADD_LIBRARY(pcraster_xsd
    STATIC
    ${SOURCES}
)
# ------------------------------------------------------------------------------
IF(${PCRTREE2_BUILD})
    # TODO allmost all test depend on PCRasterXSD they have to
    # be rewritten...................
    ADD_CUSTOM_COMMAND(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/unitTestXSD.cc
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/unitTest.xsd ${XSD_EPILOGUE}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT Generating ${CMAKE_CURRENT_BINARY_DIR}/unitTestXSD.cc
      COMMAND ${XSD_EXECUTABLE} ${XSD_OPTIONS}
          ${CMAKE_CURRENT_SOURCE_DIR}/unitTest.xsd
    )
    SET(TEST_SOURCES
      pcrxsd_commontypestest.cc
      pcrxsd_dominputtest.cc
      pcrxsd_DomAlgorithmTest.cc
      pcrxsd_test.cc
      pcrxsd_utilstest.cc
      pcrxsd_xsdtest.cc
      ${CMAKE_CURRENT_BINARY_DIR}/unitTestXSD.cc
    )
    ADD_EXECUTABLE(test_pcraster_xsd
        ${TEST_SOURCES}
    )
    TARGET_LINK_LIBRARIES(test_pcraster_xsd
        pcraster_xsd
        utils
        ${XercesC_LIBRARIES}
        ${Boost_SYSTEM_LIBRARY}
    )
    ADD_CUSTOM_TARGET(run_test_pcraster_xsd
        COMMAND ${CMAKE_COMMAND} -E copy_directory
            ${CMAKE_CURRENT_SOURCE_DIR}/testdata
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/unitTest.xsd
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/../xmlschema/commonTypes.xsd
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND test_pcraster_xsd
        DEPENDS test_pcraster_xsd
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Running pcraster_xsd unit tests" VERBATIM
    )
    ADD_DEPENDENCIES(tests
        run_test_pcraster_xsd
    )
ENDIF()
