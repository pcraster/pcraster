set(CMAKE_C_FLAGS
    "${CMAKE_C_FLAGS} /DWIN32 /D_WINDOWS /W3 /wd4267 /wd4251"
)

set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} /DWIN32 /D_WINDOWS /W3 /GR /EHsc /wd4267 /wd4251"
)


set(CMAKE_C_FLAGS_RELEASE
    "${CMAKE_C_FLAGS_RELEASE} /O2"
)

set(CMAKE_CXX_FLAGS_RELEASE
    "${CMAKE_CXX_FLAGS_RELEASE} /O2"
)


set(CMAKE_C_FLAGS_RELWITHDEBINFO
    "${CMAKE_C_FLAGS_RELWITHDEBINFO} /O2"
)

set(CMAKE_CXX_FLAGS_RELWITHDEBINFO
    "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /O2"
)


set(CMAKE_C_FLAGS_DEBUG
    "${CMAKE_C_FLAGS_DEBUG} /Od"
)

set(CMAKE_CXX_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} /Od"
)

add_compile_definitions(
    # -D_SCL_SECURE_NO_WARNINGS
    _CRT_SECURE_NO_WARNINGS
    _USE_MATH_DEFINES
    NOMINMAX
    # -D_ALLOW_KEYWORD_MACROS
)


# /wd4101

# if(MSVC)
#     # TODO add debug/release flags?
#
#     # Get rid of the min and max macros.
#     # Refactor the define private/protected public stuff (allow keywords macro)
#     add_compile_definitions(
#         -D_SCL_SECURE_NO_WARNINGS
#         -D_CRT_SECURE_NO_WARNINGS
#         -D_USE_MATH_DEFINES
#         -DNOMINMAX
#         -D_ALLOW_KEYWORD_MACROS
#     )
#
#
#     # add /w3
#     # disable these warnings?
#     set(CMAKE_CXX_FLAGS
#         "${CMAKE_CXX_FLAGS} /std:c++14 /wd4244 /wd4396 /wd4305"
#     )
#
# endif()
