macro(force_out_of_tree_build)
    string(COMPARE EQUAL "${CMAKE_SOURCE_DIR}" "${CMAKE_BINARY_DIR}"
        in_source_build)
    if(in_source_build)
        message(FATAL_ERROR "PCRaster must be built out-of-source")
    endif()
endmacro()


# TODO Can we somehow configure the extension to end up in bin/python instead
# TODO of bin? Currently we cannot have a dll and a python extension
# TODO both named bla. On Windows the import lib of the python extension will
# TODO conflict with the import lib of the dll.
MACRO(CONFIGURE_PYTHON_EXTENSION EXTENTION_TARGET EXTENSION_NAME)
    SET_TARGET_PROPERTIES(${EXTENTION_TARGET}
        PROPERTIES
            OUTPUT_NAME "${EXTENSION_NAME}"
    )

    # Configure suffix and prefix, depending on the Python OS conventions.
    SET_TARGET_PROPERTIES(${EXTENTION_TARGET}
        PROPERTIES
            PREFIX ""
    )

    IF(WIN32)
        SET_TARGET_PROPERTIES(${EXTENTION_TARGET}
            PROPERTIES
                DEBUG_POSTFIX "_d"
                SUFFIX ".pyd"
        )
    ELSE(WIN32)
        SET_TARGET_PROPERTIES(${EXTENTION_TARGET}
            PROPERTIES
                SUFFIX ".so"
        )
    ENDIF(WIN32)
ENDMACRO()


# ADD_FILE_DEPENDENCY 
# does some additonial stuff compared to CMake's SET_SOURCE_FILES_PROPERTIES:
# *extends* an existing dependency list of a TARGET
# while SET_SOURCE_FILES_PROPERTIES seems to *overwrite* that list
# use ADD_FILE_DEPENDENCY only if SET_SOURCE_FILES_PROPERTIES does not
# work for the case at hand
MACRO(ADD_FILE_DEPENDENCY TARGET)
    GET_SOURCE_FILE_PROPERTY(CURRENT_DEPENDENCIES ${TARGET} OBJECT_DEPENDS)

    IF(CURRENT_DEPENDENCIES STREQUAL NOTFOUND)
        SET(NEW_DEPENDENCIES ${ARGN})
    ELSE(CURRENT_DEPENDENCIES STREQUAL NOTFOUND)
        SET(NEW_DEPENDENCIES ${CURRENT_DEPENDENCIES} ${ARGN})
    ENDIF(CURRENT_DEPENDENCIES STREQUAL NOTFOUND)

    SET_SOURCE_FILES_PROPERTIES(${TARGET}
        PROPERTIES OBJECT_DEPENDS "${NEW_DEPENDENCIES}")

    # SET(${TARGET}_deps ${${TARGET}_deps} ${ARGN})
    # SET_SOURCE_FILES_PROPERTIES(${TARGET}
    #   PROPERTIES OBJECT_DEPENDS "${${TARGET}_deps}")
ENDMACRO(ADD_FILE_DEPENDENCY)
