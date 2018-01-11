# ADD_FILE_DEPENDENCY
# does some additional stuff compared to CMake's SET_SOURCE_FILES_PROPERTIES:
# *extends* an existing dependency list of a TARGET
# while SET_SOURCE_FILES_PROPERTIES seems to *overwrite* that list
# use ADD_FILE_DEPENDENCY only if SET_SOURCE_FILES_PROPERTIES does not
# work for the case at hand
macro(add_file_dependency TARGET)
    get_source_file_property(CURRENT_DEPENDENCIES ${TARGET} OBJECT_DEPENDS)

    if(CURRENT_DEPENDENCIES STREQUAL NOTFOUND)
        set(NEW_DEPENDENCIES ${ARGN})
    else(CURRENT_DEPENDENCIES STREQUAL NOTFOUND)
        set(NEW_DEPENDENCIES ${CURRENT_DEPENDENCIES} ${ARGN})
    endif(CURRENT_DEPENDENCIES STREQUAL NOTFOUND)

    set_source_files_properties(${TARGET}
        PROPERTIES OBJECT_DEPENDS "${NEW_DEPENDENCIES}")

    # SET(${TARGET}_deps ${${TARGET}_deps} ${ARGN})
    # SET_SOURCE_FILES_PROPERTIES(${TARGET}
    #   PROPERTIES OBJECT_DEPENDS "${${TARGET}_deps}")
endmacro(add_file_dependency)

macro(fix_boost_python_visibility TARGET)
    # Hidden visibility flags will fail to import a Python module
    if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        if(${Boost_MINOR_VERSION} EQUAL 65)
            target_compile_definitions(
                ${TARGET}
            PUBLIC
                -DBOOST_PYTHON_USE_GCC_SYMBOL_VISIBILITY)
        endif()
    endif()
endmacro(fix_boost_python_visibility)
