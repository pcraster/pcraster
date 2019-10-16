
# set(CMAKE_C_COMPILER )
# set(CMAKE_CXX_COMPILER )



set(CMAKE_C_FLAGS
    "${CMAKE_C_FLAGS} /DWIN32 /D_WINDOWS /W3 /O2 /wd4267 /wd4251"
)

set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} /DWIN32 /D_WINDOWS /W3 /GR /EHsc /O2 /std:c++14 /wd4267 /wd4251"
)


add_compile_definitions(
    # -D_SCL_SECURE_NO_WARNINGS
    _CRT_SECURE_NO_WARNINGS
    _USE_MATH_DEFINES
    NOMINMAX
    # -D_ALLOW_KEYWORD_MACROS
)


set(CMAKE_DEBUG_POSTFIX "d")


set(CMAKE_C_FLAGS_DEBUG
    "${CMAKE_C_FLAGS_DEBUG} /DWIN32 /D_WINDOWS /W3 /Od"
)

set(CMAKE_CXX_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} /DWIN32 /D_WINDOWS /W3 /GR /EHsc /Od /std:c++14"
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
#     set(CMAKE_C_FLAGS
#         "${CMAKE_C_FLAGS} /wd4267"
#     )
#
#     # add /w3
#     # disable these warnings?
#     set(CMAKE_CXX_FLAGS
#         "${CMAKE_CXX_FLAGS} /std:c++14 /wd4267 /wd4251 /wd4244 /wd4396 /wd4305"
#     )
#
# endif()
