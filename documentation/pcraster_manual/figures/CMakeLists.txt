# files are just hardcoded here
# actually they should be 'grepped' from the manual xml
# FTTB we assume no changes in the set of files
#
# Manually update the PNGs in case you modify the EPS files!
#
SET(EPS_FIGURES
    block.eps
    demldd.eps
    dynvb1.eps
    dynvb2.eps
    fric.eps
    lddcode.eps
    locattr.eps
    opaccu.eps
    opalg.eps
    oparea.eps
    opmap.eps
    oppath.eps
    oppoint.eps
    opsysacc.eps
    opwin.eps
    seqconc.eps
    stack.eps
    stackrel.eps
)

SET(PNG_FIGURES
    block.png
    demldd.png
    dynvb1.png
    dynvb2.png
    fric.png
    lddcode.png
    locattr.png
    opaccu.png
    opalg.png
    oparea.png
    opmap.png
    oppath.png
    oppoint.png
    opsysacc.png
    opwin.png
    seqconc.png
    stack.png
    stackrel.png
)

FOREACH(FILENAME IN LISTS PNG_FIGURES)
    SET(PNG_FIGURE ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME})
    ADD_CUSTOM_COMMAND (
        OUTPUT  ${PNG_FIGURE}
        COMMAND ${CMAKE_COMMAND}
        ARGS    -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
            ${CMAKE_CURRENT_BINARY_DIR}/${in_file}
    )
ENDFOREACH()

ADD_CUSTOM_TARGET(preprocessFigures
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_CURRENT_SOURCE_DIR}/DynamicWaveChannel.png
        ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${PNG_FIGURES}
)
