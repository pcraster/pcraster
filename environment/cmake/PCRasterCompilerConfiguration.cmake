# disabling compiler warnings
# these ones should be fixed eventually...
# some of these flags are covered by github issue
# some of these need to be ticketed

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")

    set(CMAKE_C_FLAGS
        "${CMAKE_C_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-unused-function -Wno-format -Wno-switch -Wno-empty-body -Wno-sign-compare"
    )
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-deprecated-declarations -Wno-ignored-qualifiers -Wno-switch -Wno-pedantic -Wno-write-strings -Wno-unused-variable -Wno-unused-local-typedefs -Wno-delete-non-virtual-dtor -Wno-unused-label -Wno-sign-compare"
    )

    # these ones should moved to devbase eventually
#     set(CMAKE_C_FLAGS
#         "${CMAKE_C_FLAGS} -march=native" # -flto
#     )
#     set(CMAKE_CXX_FLAGS
#         "${CMAKE_CXX_FLAGS} -march=native" # -flto
#     )
    # to make flto work set ar and ranlib
    # SET(CMAKE_AR gcc-ar)
    # SET(CMAKE_RANLIB gcc-ranlib)

endif()
