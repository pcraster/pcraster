# INPUTS:
#  only the following are expected to be present (the rest is generated):
#
# .itxt   ascii formatted input map see AscHeader2map script for format
#         all manual example input are created and kept as .itxt
#         in the test commands they are input as .imap
#         make will pick up all .itxt files in this directory (MAP_INPUTS)
# .txt    a number of input tables, set here to TEXT_INPUTS,
#         except for those generated in this Makefile as defined by TEXT_OUTPUTS

CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/AscHeader2map.in
    ${CMAKE_CURRENT_BINARY_DIR}/AscHeader2map
    @ONLY
    NEWLINE_STYLE UNIX
)

CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/PCRasterSample.py.in
    ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py
    @ONLY
    NEWLINE_STYLE UNIX
)

CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/check_man_pages.py.in
    ${CMAKE_CURRENT_BINARY_DIR}/check_man_pages.py
    @ONLY
    NEWLINE_STYLE UNIX
)

ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/examples.sentinel

    # CMAKE_CFG_INTDIR is set at build-time, not at configure-time. Search
    # and replace this string at build-time.
    COMMAND sed -i.tmp "s/CMAKE_CFG_INTDIR/${CMAKE_CFG_INTDIR}/g"
        ${CMAKE_CURRENT_BINARY_DIR}/AscHeader2map
    COMMAND sed -i.tmp "s/CMAKE_CFG_INTDIR/${CMAKE_CFG_INTDIR}/g"
        ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py

    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/CmdSamples.py
        ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/make_table.py
        ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/ScriptSamples.py
        ${CMAKE_CURRENT_BINARY_DIR}

    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/*.itxt ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/*.txt ${CMAKE_CURRENT_BINARY_DIR}

    COMMAND ${PYTHON_EXECUTABLE}
        ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py script
        --format=makefile ${CMAKE_CURRENT_BINARY_DIR}/generatedMakefile

    COMMAND make -f ${CMAKE_CURRENT_BINARY_DIR}/generatedMakefile
        testInputFiles

    COMMAND ${PYTHON_EXECUTABLE}
        ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py script
        --format=cpp ${CMAKE_CURRENT_BINARY_DIR}/cpptest.cc
    COMMAND ${PYTHON_EXECUTABLE}
        ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py script
        --format=python
        ${CMAKE_CURRENT_BINARY_DIR}/testexamples.py
    COMMAND ${PYTHON_EXECUTABLE}
        ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py script
        --format=multicore
        ${CMAKE_CURRENT_BINARY_DIR}/testexamples_multicore.py

    COMMAND touch ${CMAKE_CURRENT_BINARY_DIR}/examples.sentinel
    DEPENDS
        asc2map
        map2asc
        map2col
        col2map
        mapattr
        pcrcalc
        table
        check_man_pages
        ${CMAKE_CURRENT_SOURCE_DIR}/CmdSamples.py
        ${CMAKE_CURRENT_SOURCE_DIR}/make_table.py
        ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py
        ${CMAKE_CURRENT_SOURCE_DIR}/ScriptSamples.py
)

ADD_CUSTOM_TARGET(generate_examples.sentinel
    DEPENDS
        check_man_pages
        ${CMAKE_CURRENT_BINARY_DIR}/examples.sentinel
)

ADD_CUSTOM_TARGET(check_man_pages
    # COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/../../../source/pcraster_model_engine/operation.xml ${CMAKE_CURRENT_BINARY_DIR}/man_page.xml
    # COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/../../../source/pcraster_model_engine/operation.dtd ${CMAKE_CURRENT_BINARY_DIR}/
    # WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    # # pass directory path containing restructured text files
    # COMMAND ${PYTHON_EXECUTABLE} check_man_pages.py
    #   ${CMAKE_CURRENT_SOURCE_DIR}/../sphinx
)


if(PCRASTER_BUILD_DOCUMENTATION)

    ADD_CUSTOM_COMMAND(
        TARGET generate_examples.sentinel
        POST_BUILD
        COMMAND make -f ${CMAKE_CURRENT_BINARY_DIR}/generatedMakefile
            manualInclusionFiles
    )

endif()
