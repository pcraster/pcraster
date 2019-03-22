
set(CMAKE_C_COMPILER )
set(CMAKE_CXX_COMPILER )




# -D_SCL_SECURE_NO_WARNINGS /wd4101


set(CMAKE_C_FLAGS
    "${PCR_C_FLAGS} "
)

set(CMAKE_CXX_FLAGS
    "${PCR_CXX_FLAGS} "
)
#
# if(WIN32)
#     set(CMAKE_DEBUG_POSTFIX "d")
# endif()

#
# if(MSVC)
#     set(CMAKE_CXX_FLAGS
#         "${CMAKE_CXX_FLAGS} -D_USE_MATH_DEFINES -DNOMINMAX")
# endif()



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
