# disabling compiler warnings
# these ones should be fixed eventually
# or moved to devbase

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")

    set(CMAKE_C_FLAGS
        "${CMAKE_C_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-unused-function -Wno-format -Wno-switch -Wno-empty-body -Wno-sign-compare"
    )
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-deprecated-declarations -Wno-ignored-qualifiers -Wno-switch -Wno-pedantic -Wno-write-strings -Wno-unused-variable -Wno-unused-local-typedefs -Wno-delete-non-virtual-dtor -Wno-unused-label -Wno-sign-compare"
    )

endif()
