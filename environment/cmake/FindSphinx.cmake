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
