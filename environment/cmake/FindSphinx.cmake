find_program(SPHINX_BUILD_EXECUTABLE
    NAMES
        sphinx-build
    HINTS
        $ENV{SPHINX_DIR}
    PATH_SUFFIXES
        bin
    DOC "Sphinx documentation generator"
)
find_program(SPHINX_APIDOC_EXECUTABLE
    NAMES
        sphinx-apidoc
    HINTS
        $ENV{SPHINX_DIR}
    PATH_SUFFIXES
        bin
    DOC "Sphinx documentation generator"
)

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(SPHINX_BUILD DEFAULT_MSG
    SPHINX_BUILD_EXECUTABLE)
find_package_handle_standard_args(SPHINX_APIDOC DEFAULT_MSG
    SPHINX_APIDOC_EXECUTABLE)

mark_as_advanced(
    SPHINX_BUILD_EXECUTABLE
    SPHINX_APIDOC_EXECUTABLE
)
