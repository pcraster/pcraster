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

# Based on http://www.cmake.org/pipermail/cmake/2011-January/041666.html
function(find_python_module module)
    string(TOUPPER ${module} module_upper)
    if(NOT PYTHON_MODULE_${module_upper})
        if(ARGC GREATER 1 AND ARGV1 STREQUAL "REQUIRED")
            set(PYTHON_MODULE_${module_upper}_FIND_REQUIRED TRUE)
        endif()
        # A module's location is usually a directory, but for binary modules
        # it's a .so file.
        execute_process(COMMAND "${PYTHON_EXECUTABLE}" "-c"
            "import re, ${module}; print(re.compile('/__init__.py.*').sub('',${module}.__file__))"
            RESULT_VARIABLE _${module}_status
            OUTPUT_VARIABLE _${module}_location
            ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
        if(NOT _${module}_status)
            set(PYTHON_MODULE_${module_upper} ${_${module}_location} CACHE STRING
                "Location of Python module ${module}")
        endif(NOT _${module}_status)
    endif(NOT PYTHON_MODULE_${module_upper})
    find_package_handle_standard_args(PYTHON_MODULE_${module_upper} DEFAULT_MSG PYTHON_MODULE_${module_upper})
endfunction(find_python_module)
