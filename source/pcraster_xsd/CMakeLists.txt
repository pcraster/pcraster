SET(AGUILA_BUILD 1)
SET(PCRTREE2_BUILD 1)

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
    # TODO --std c++11  # unique_ptr instead of auto_ptr
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

IF(AGUILA_BUILD)
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
        COMMAND ${CMAKE_COMMAND} -E copy ${_AGUILA_XSD} ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${CMAKE_COMMAND} -E copy ${XSD_SRC_DIR}/commonTypes.xsd
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/xsdEpilogue.txt
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${XSD_EXECUTABLE} ${XSD_OPTIONS} ${AGUILA_RES}
            --hxx-epilogue-file xsdEpilogue.txt Aguila.xsd
    )

    TEXT_TO_CHARACTER_ARRAY(
        ${_AGUILA_XSD}
        compiled_Aguila
    )
ENDIF()

IF(PCRTREE2_BUILD)
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
        COMMAND ${CMAKE_COMMAND} -E copy ${_PCRASTER_XSD} ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${CMAKE_COMMAND} -E copy ${XSD_SRC_DIR}/commonTypes.xsd
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/xsdEpilogue.txt
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

ADD_LIBRARY(pcraster_xsd STATIC
    ${SOURCES}
)

add_library(pcraster::xsd ALIAS pcraster_xsd)

target_include_directories(pcraster_xsd
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcraster_xsd
    PUBLIC
        pcraster::utils
)

# ------------------------------------------------------------------------------
IF(PCRTREE2_BUILD)
    if(PCRASTER_BUILD_TEST)
        # TODO allmost all test depend on PCRasterXSD they have to
        # be rewritten...................
        ADD_CUSTOM_COMMAND(
          OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/unitTestXSD.cc
          DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/unitTest.xsd ${XSD_EPILOGUE}
          WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
          COMMAND ${XSD_EXECUTABLE} ${XSD_OPTIONS}
              ${CMAKE_CURRENT_SOURCE_DIR}/unitTest.xsd
        )
        add_unit_tests(
            SCOPE pcraster_xsd
            NAMES
                pcrxsd_commontypestest
                pcrxsd_dominputtest
                pcrxsd_DomAlgorithmTest
                pcrxsd_utilstest
                pcrxsd_xsdtest
            SUPPORT_NAMES
                ${CMAKE_CURRENT_BINARY_DIR}/unitTestXSD.cc
            LINK_LIBRARIES
                pcraster::xsd
            DEPENDENCIES
                pcraster_xsd_test_run_epilog
        )
        add_custom_target(pcraster_xsd_test_run_epilog
            COMMAND ${CMAKE_COMMAND} -E copy_directory
                ${CMAKE_CURRENT_SOURCE_DIR}/testdata
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy
                ${CMAKE_CURRENT_SOURCE_DIR}/unitTest.xsd
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy
                ${CMAKE_CURRENT_SOURCE_DIR}/../xmlschema/commonTypes.xsd
                ${CMAKE_CURRENT_BINARY_DIR}
        )
    endif()
ENDIF()
