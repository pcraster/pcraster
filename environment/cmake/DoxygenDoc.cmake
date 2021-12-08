# Only generate the TODO page in Debug configurations. The idea is not to
# install a TODO list page in the API docs when releasing a project.

file (GENERATE
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile_$<CONFIG>
    CONTENT "
    PROJECT_NAME            = PCRaster
    ALWAYS_DETAILED_SEC     = YES
    BUILTIN_STL_SUPPORT     = YES
    CLASS_DIAGRAMS          = YES
    ENABLE_PREPROCESSING    = YES
    EXCLUDE_PATTERNS        = *Test.h *Test.cc *_test.cc
    EXPAND_ONLY_PREDEF      = NO
    EXTRACT_ALL             = YES
    EXTRACT_PRIVATE         = NO
    EXTRACT_STATIC          = NO
    FILE_PATTERNS           = *.h *.hpp *.hxx *.c *.cc *.cpp *.cxx *.dox *.md
    FULL_PATH_NAMES         = YES
    GENERATE_LATEX          = NO
    GENERATE_TODOLIST       = $<IF:$<CONFIG:Release>,NO,YES>
    INPUT                   = ${CMAKE_CURRENT_SOURCE_DIR}
    HAVE_DOT                = YES
    DOT_GRAPH_MAX_NODES     = 75
    INCLUDE_GRAPH           = YES
    INHERIT_DOCS            = YES
    INLINE_INFO             = YES
    INLINE_INHERITED_MEMB   = YES
    MATHJAX_EXTENSIONS      = TeX/AMSmath TeX/AMSsymbols
    QUIET                   = YES
    RECURSIVE               = YES
    SEARCH_INCLUDES         = YES
    SHOW_FILES              = YES
    SHOW_USED_FILES         = NO
    SHOW_GROUPED_MEMB_INC   = YES
    SORT_MEMBER_DOCS        = NO
    SOURCE_BROWSER          = NO
    STRIP_FROM_INC_PATH     = ${CMAKE_CURRENT_SOURCE_DIR}
    STRIP_FROM_PATH         = ${CMAKE_CURRENT_SOURCE_DIR}
    TEMPLATE_RELATIONS      = YES
    USE_MATHJAX             = YES
    VERBATIM_HEADERS        = NO
    WARN_IF_DOC_ERROR       = YES
    WARN_IF_UNDOCUMENTED    = NO  # Because EXTRACT_ALL is turned on.
    WARNINGS                = YES
    WARN_NO_PARAMDOC        = YES
    OUTPUT_DIRECTORY        = ${PROJECT_BINARY_DIR}/api_docs/$<CONFIG>
    "
)

add_custom_command(
    OUTPUT ${PROJECT_BINARY_DIR}/api_docs/$<CONFIG>/html/index.html
    COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile_$<CONFIG>
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile_$<CONFIG>
)

# This target should be built by default, because otherwise the results won't
# be available when the user wants to install the generated documentation
add_custom_target(cpp_doc ALL
    DEPENDS ${PROJECT_BINARY_DIR}/api_docs/$<CONFIG>/html/index.html
)
