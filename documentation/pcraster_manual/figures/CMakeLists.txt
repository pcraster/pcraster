# files are just hardcoded here
# actually they should be 'grepped' from the manual xml
# FTTB we assume no changes in the set of files
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
SET(PNG_FIGURES)
FOREACH(EPS_FIGURE IN LISTS EPS_FIGURES)
    GET_FILENAME_COMPONENT(FILENAME ${EPS_FIGURE} NAME_WE)
    SET(PNG_FIGURE ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}.png)
    ADD_CUSTOM_COMMAND(
        OUTPUT ${PNG_FIGURE}
        COMMAND ${ImageMagick_convert_EXECUTABLE}
            ${CMAKE_CURRENT_SOURCE_DIR}/${EPS_FIGURE} ${PNG_FIGURE}
        DEPENDS ${EPS_FIGURE}
    )
    LIST(APPEND PNG_FIGURES ${PNG_FIGURE})
ENDFOREACH()

ADD_CUSTOM_TARGET(preprocessFigures
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_CURRENT_SOURCE_DIR}/DynamicWaveChannel.png
        ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${PNG_FIGURES}
)
