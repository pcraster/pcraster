# Add a test target.
# Also configures the environment to point to the location of shared libs.
# The idea of this is to keep the dev's shell as clean as possible. Use
# ctest command to run unit tests.
#
# SCOPE: Some prefix. Often the lib name of the lib being tested
# NAME : Name of test module, with extension
# UTF_ARGUMENTS_SEPARATOR: String to put between the command and the
#     UTF arguments.
#     TODO: This is how it could work:
#     <command> <runtime_arguments>
#         <utf_arguments_separator> <utf_arguments>
#         <command_arguments_separator> <command_arguments>
#     HPX: utf_arguments_separator == '--'
#     UTF: command_arguments_separator == '--'
#         (so this separator doesn't have to be passed in!)
# LINK_LIBRARIES: Libraries to link against
# DEPENDENCIES: Targets this test target depends on
# ENVIRONMENT: Environment variables that should be defined for running
#     the test
macro(add_unit_test)
    set(OPTIONS "")
    set(ONE_VALUE_ARGUMENTS SCOPE NAME UTF_ARGUMENTS_SEPARATOR)
    set(MULTI_VALUE_ARGUMENTS
        SUPPORT_NAMES
        INCLUDE_DIRS
        OBJECT_LIBRARIES
        LINK_LIBRARIES
        DEPENDENCIES
        ENVIRONMENT
    )

    cmake_parse_arguments(ADD_UNIT_TEST "${OPTIONS}" "${ONE_VALUE_ARGUMENTS}"
        "${MULTI_VALUE_ARGUMENTS}" ${ARGN})

    if(ADD_UNIT_TEST_UNPARSED_ARGUMENTS)
        message(FATAL_ERROR
            "Macro called with unrecognized arguments: "
            "${ADD_UNIT_TEST_UNPARSED_ARGUMENTS}"
        )
    endif()

    set(TEST_MODULE_NAME ${ADD_UNIT_TEST_NAME})
    cmake_path(REMOVE_EXTENSION TEST_MODULE_NAME OUTPUT_VARIABLE TEST_MODULE_NAME_STEM)
    set(TEST_EXE_NAME ${ADD_UNIT_TEST_SCOPE}_${TEST_MODULE_NAME_STEM})
    string(REPLACE "/" "_" TEST_EXE_NAME ${TEST_EXE_NAME})

    add_executable(${TEST_EXE_NAME} ${TEST_MODULE_NAME}
        ${ADD_UNIT_TEST_SUPPORT_NAMES}
        ${ADD_UNIT_TEST_OBJECT_LIBRARIES})
    target_compile_definitions(${TEST_EXE_NAME}
        PRIVATE
            BOOST_TEST_DYN_LINK
    )
    target_include_directories(${TEST_EXE_NAME} SYSTEM
        PRIVATE
            ${Boost_INCLUDE_DIRS})
    target_include_directories(${TEST_EXE_NAME}
        PRIVATE
            ${ADD_UNIT_TEST_INCLUDE_DIRS})
    target_link_libraries(${TEST_EXE_NAME}
        PRIVATE
            ${ADD_UNIT_TEST_LINK_LIBRARIES}
            Boost::unit_test_framework)

    add_test(NAME ${TEST_EXE_NAME}
        # catch_system_errors: Prevent UTF to detect system errors. This
        #     messes things up when doing system calls to Python unit tests.
        #     See also: http://lists.boost.org/boost-users/2009/12/55048.php
        COMMAND ${TEST_EXE_NAME} ${ADD_UNIT_TEST_UTF_ARGUMENTS_SEPARATOR}
            --catch_system_errors=no
    )

    if(ADD_UNIT_TEST_DEPENDENCIES)
        ADD_DEPENDENCIES(${TEST_EXE_NAME} ${ADD_UNIT_TEST_DEPENDENCIES})
    endif()

    # Maybe add ${EXECUTABLE_OUTPUT_PATH} in the future. If needed.
    set(PATH_LIST $ENV{PATH})
    # list(INSERT PATH_LIST 0 ${Boost_LIBRARY_DIRS})
    set(PATH_STRING "${PATH_LIST}")

    if(${CMAKE_HOST_SYSTEM_NAME} STREQUAL "Windows")
        string(REPLACE "\\" "/" PATH_STRING "${PATH_STRING}")
        string(REPLACE ";" "\\;" PATH_STRING "${PATH_STRING}")
    else()
        string(REPLACE ";" ":" PATH_STRING "${PATH_STRING}")
    endif()

    # set_target_properties(${TEST_EXE_NAME}
    #     PROPERTIES
    #         EXCLUDE_FROM_ALL 1
    #         EXCLUDE_FROM_DEFAULT_BUILD 1
    # )
    set_tests_properties(${TEST_EXE_NAME}
        PROPERTIES
            ENVIRONMENT
                "PATH=${PATH_STRING};${ADD_UNIT_TEST_ENVIRONMENT}"
    )
endmacro()


function(add_unit_tests)
    set(OPTIONS "")
    set(ONE_VALUE_ARGUMENTS SCOPE UTF_ARGUMENTS_SEPARATOR)
    set(MULTI_VALUE_ARGUMENTS
        NAMES
        SUPPORT_NAMES
        INCLUDE_DIRS
        OBJECT_LIBRARIES
        LINK_LIBRARIES
        DEPENDENCIES
        ENVIRONMENT
    )

    cmake_parse_arguments(ADD_UNIT_TESTS "${OPTIONS}" "${ONE_VALUE_ARGUMENTS}"
        "${MULTI_VALUE_ARGUMENTS}" ${ARGN})

    if(ADD_UNIT_TESTS_UNPARSED_ARGUMENTS)
        message(FATAL_ERROR
            "Macro called with unrecognized arguments: "
            "${ADD_UNIT_TESTS_UNPARSED_ARGUMENTS}"
        )
    endif()

    foreach(NAME ${ADD_UNIT_TESTS_NAMES})
        add_unit_test(
            SCOPE ${ADD_UNIT_TESTS_SCOPE}
            NAME ${NAME}
            UTF_ARGUMENTS_SEPARATOR ${ADD_UNIT_TESTS_UTF_ARGUMENTS_SEPARATOR}
            SUPPORT_NAMES ${ADD_UNIT_TESTS_SUPPORT_NAMES}
            INCLUDE_DIRS ${ADD_UNIT_TESTS_INCLUDE_DIRS}
            OBJECT_LIBRARIES ${ADD_UNIT_TESTS_OBJECT_LIBRARIES}
            LINK_LIBRARIES ${ADD_UNIT_TESTS_LINK_LIBRARIES}
            DEPENDENCIES ${ADD_UNIT_TESTS_DEPENDENCIES}
            ENVIRONMENT ${ADD_UNIT_TESTS_ENVIRONMENT}
        )
    endforeach()
endfunction()
