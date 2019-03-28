# WIP



# set(CMAKE_C_FLAGS_INIT "" CACHE STRING "")
# set(CMAKE_CXX_FLAGS_INIT "" CACHE STRING "")


# set(CMAKE_C_FLAGS_DEBUG   "${CMAKE_C_FLAGS_DEBUG} -U_FORTIFY_SOURCE")
# set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -U_FORTIFY_SOURCE")


set(CMAKE_SKIP_BUILD_RPATH FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)


# Make linker report any unresolved symbols.
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--no-undefined")




set(PCR_C_FLAGS
  "-march=native -Wall -Wextra -Wno-pedantic -Wwrite-strings -Werror=strict-aliasing -fno-strict-overflow -Wno-implicit-fallthrough -Wno-cpp -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-unused-function -Wno-switch -Wno-empty-body -Wno-sign-compare -Wno-float-equal -Wno-implicit-function-declaration -Wno-cast-function-type -Wno-unused-variable"
)

set(PCR_C_FLAGS_GCC7
  ""
)

set(PCR_C_FLAGS_GCC8
  "-Wmultistatement-macros"
)



set(PCR_CXX_FLAGS
  "-march=native -Wall -Wextra -Wno-pedantic -Wwrite-strings -Werror=strict-aliasing -ftemplate-backtrace-limit=0 -fno-strict-overflow -Wno-unused-variable -Wno-unused-result -Wno-implicit-fallthrough  -Wno-cpp -Wno-parentheses -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-ignored-qualifiers -Wno-switch -Wno-write-strings -Wno-unused-local-typedefs -Wno-delete-non-virtual-dtor -Wno-unused-label -Wno-sign-compare -Wno-float-equal -Wno-misleading-indentation -Wno-deprecated-declarations -Wduplicated-cond  -Wno-null-dereference  -Wno-useless-cast -Wno-old-style-cast -Wno-double-promotion -Wformat=2 -Wno-shadow -Wno-format-nonliteral -Wno-suggest-final-types -Wno-suggest-final-methods -Wno-cast-function-type"
)

#  -Wno-catch-value

set(PCR_CXX_FLAGS_GCC7
  "-Wno-duplicated-branches "
)

# flto with -Wodr (no)

# -Wcast-align -Woverloaded-virtual -Wsign-conversion missing-include-dirs


set(PCR_CXX_FLAGS_GCC8
  "-Wno-catch-value"
)


