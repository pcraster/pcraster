find_package(Sphinx REQUIRED)


function(create_sphinx_docs)
    set(OPTIONS "")
    set(ONE_VALUE_ARGUMENTS TARGET)
    set(MULTI_VALUE_ARGUMENTS SOURCES)

    cmake_parse_arguments(SPHINX_DOCS "${OPTIONS}" "${ONE_VALUE_ARGUMENTS}"
        "${MULTI_VALUE_ARGUMENTS}" ${ARGN})

    if(SPHINX_DOCS_UNPARSED_ARGUMENTS)
        message(FATAL_ERROR
            "Macro called with unrecognized arguments: "
            "${SPHINX_DOCS_UNPARSED_ARGUMENTS}")
    endif()


    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/conf.py.in
        ${CMAKE_CURRENT_BINARY_DIR}/conf.py
    )
    configure_file(
        # Don't name the new file Makefile, as it conflicts with the
        # CMake generated file.
        ${CMAKE_CURRENT_SOURCE_DIR}/Makefile.in
        ${CMAKE_CURRENT_BINARY_DIR}/Makefile-sphinx
        @ONLY
    )

    foreach(NAME ${SPHINX_DOCS_SOURCES})
        set(SPHINX_SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/${NAME})
        set(COPIED_SPHINX_SOURCE ${CMAKE_CURRENT_BINARY_DIR}/${NAME})
        add_custom_command(
            OUTPUT ${COPIED_SPHINX_SOURCE}
            COMMAND ${CMAKE_COMMAND} -E copy ${SPHINX_SOURCE}
                ${COPIED_SPHINX_SOURCE}
            DEPENDS ${SPHINX_SOURCE}
        )
        list(APPEND COPIED_SPHINX_SOURCES ${COPIED_SPHINX_SOURCE})
    endforeach()

    foreach(NAME _build _static _templates)
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${NAME}
            COMMAND ${CMAKE_COMMAND} -E make_directory
                ${CMAKE_CURRENT_BINARY_DIR}/${NAME}
        )
    endforeach()

    set(SPHINX_SPHINXOPTS "-q -W")
    # set(SPHINX_PAPER a4)

    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/_build/html/index.html

        # Doesn't work when CMAKE_MAKE_PROGRAM is not Make...
        # COMMAND ${CMAKE_MAKE_PROGRAM}
        # TODO: First find GNU Make
        COMMAND make
            SPHINXOPTS=${SPHINX_SPHINXOPTS}
            -C ${CMAKE_CURRENT_BINARY_DIR}
            -f Makefile-sphinx html
        DEPENDS
            ${CMAKE_CURRENT_BINARY_DIR}/conf.py
            ${CMAKE_CURRENT_BINARY_DIR}/Makefile-sphinx
            ${CMAKE_CURRENT_BINARY_DIR}/_build
            ${CMAKE_CURRENT_BINARY_DIR}/_static
            ${CMAKE_CURRENT_BINARY_DIR}/_templates
            ${COPIED_SPHINX_SOURCES}
    )

    add_custom_target(${SPHINX_DOCS_TARGET} ALL
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/_build/html/index.html
    )
endfunction()


function(document_python_api)
    set(OPTIONS "")
    set(ONE_VALUE_ARGUMENTS TARGET SOURCE_DIRECTORY BINARY_DIRECTORY)
    set(MULTI_VALUE_ARGUMENTS EXCLUDE_DIRECTORIES)

    cmake_parse_arguments(PYTHON_API "${OPTIONS}" "${ONE_VALUE_ARGUMENTS}"
        "${MULTI_VALUE_ARGUMENTS}" ${ARGN})

    if(PYTHON_API_UNPARSED_ARGUMENTS)
        message(FATAL_ERROR
            "Macro called with unrecognized arguments: "
            "${PYTHON_API_UNPARSED_ARGUMENTS}")
    endif()

    add_custom_command(
        OUTPUT ${PYTHON_API_BINARY_DIRECTORY}/modules.rst
        COMMAND ${SPHINX_APIDOC_EXECUTABLE} -o ${PYTHON_API_BINARY_DIRECTORY}
            --separate -H ${PROJECT_NAME}
            # -V "${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}"
            # -R ${${PROJECT_NAME}_VERSION}
            ${PYTHON_API_SOURCE_DIRECTORY} ${EXCLUDE_DIRECTORIES}
        DEPENDS
            ${PYTHON_API_SOURCE_DIRECTORY}/__init__.py
    )

    add_custom_target(${PYTHON_API_TARGET} ALL
        DEPENDS ${PYTHON_API_BINARY_DIRECTORY}/modules.rst)

    # TODO How to determine which files will be generated by apidoc?
    # file(GLOB GENERATED_SPHINX_SOURCES
    #     ${PYTHON_API_BINARY_DIRECTORY}/*.rst
    # )
    # set_directory_properties(
    #     PROPERTIES
    #         ADDITIONAL_MAKE_CLEAN_FILES
    #             "${COPIED_SPHINX_SOURCES}"
    # )
endfunction()