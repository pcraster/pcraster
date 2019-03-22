
set(CMAKE_C_COMPILER clang-7)
set(CMAKE_CXX_COMPILER clang++-7)


include(${CMAKE_CURRENT_LIST_DIR}/gcc.cmake)


set(CMAKE_C_FLAGS
     "-march=native -Wall -Wextra "
)
#
set(CMAKE_CXX_FLAGS
     "-march=native -Wall -Wextra "
)

if(APPLE)
  set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
  set(CMAKE_INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib")
endif()


# if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
#
#     set(CMAKE_C_FLAGS
#         "${CMAKE_C_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual  -Wno-unused-parameter -Wno-empty-body -Wno-unused-function -Wno-format -Wno-switch -Wno-empty-body -Wno-sign-compare -Wno-float-equal   "
#     )
#     set(CMAKE_CXX_FLAGS
#         "${CMAKE_CXX_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual  -Wno-unused-parameter -Wno-empty-body  -Wno-ignored-qualifiers -Wno-switch -Wno-pedantic -Wno-write-strings -Wno-unused-variable -Wno-unused-local-typedefs -Wno-delete-non-virtual-dtor -Wno-unused-label -Wno-sign-compare -Wno-float-equal -Wno-overloaded-virtual -Wno-unused-result -Wno-sometimes-uninitialized -Wno-undefined-var-template  "
#     )
#
# endif()


