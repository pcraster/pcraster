# work in progress file for compiler flags

# disabling compiler warnings
# these ones should be fixed eventually...
# some of these flags are covered by github issue
# some of these need to be ticketed

# move useful warnings to devbase later on


if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")

    set(CMAKE_C_FLAGS
        "${CMAKE_C_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual  -Wno-unused-parameter -Wno-empty-body -Wno-unused-function -Wno-format -Wno-switch -Wno-empty-body -Wno-sign-compare -Wno-float-equal   "
    )
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual  -Wno-unused-parameter -Wno-empty-body  -Wno-ignored-qualifiers -Wno-switch -Wno-pedantic -Wno-write-strings -Wno-unused-variable -Wno-unused-local-typedefs -Wno-delete-non-virtual-dtor -Wno-unused-label -Wno-sign-compare -Wno-float-equal -Wno-overloaded-virtual -Wno-unused-result -Wno-sometimes-uninitialized -Wno-undefined-var-template  "
    )

endif()




if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")

    set(CMAKE_C_FLAGS
        "${CMAKE_C_FLAGS} -Wno-implicit-fallthrough -Wno-pedantic -Wno-cpp -Wno-parentheses -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-unused-function -Wno-format -Wno-switch -Wno-empty-body -Wno-sign-compare -Wno-float-equal -Wno-misleading-indentation"
    )
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -Wno-unused-result -Wno-implicit-fallthrough  -Wno-cpp -Wno-parentheses -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-ignored-qualifiers -Wno-switch -Wno-write-strings -Wno-unused-variable -Wno-unused-local-typedefs -Wno-delete-non-virtual-dtor -Wno-unused-label -Wno-sign-compare -Wno-float-equal -Wno-misleading-indentation -Wno-deprecated-declarations -Wno-pedantic"
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

if(MSVC)
    # TODO add debug/release flags?

    # Get rid of the min and max macros.
    # Refactor the define private/protected public stuff (allow keywords macro)
    add_definitions(
        -D_SCL_SECURE_NO_WARNINGS
        -D_CRT_SECURE_NO_WARNINGS
        -D_USE_MATH_DEFINES
        -DNOMINMAX
        -D_ALLOW_KEYWORD_MACROS
    )

    set(CMAKE_C_FLAGS
        "${CMAKE_C_FLAGS} /wd4267"
    )

    # add /w3
    # disable these warnings?
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} /std:c++14 /wd4267 /wd4251 /wd4244 /wd4396 /wd4305"
    )

endif()
