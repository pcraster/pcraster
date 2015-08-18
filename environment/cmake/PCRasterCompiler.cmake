set(CMAKE_CXX_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG -DDEBUG_BUILD -DDEBUG_DEVELOP"
)
set(CMAKE_C_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG -DDEBUG_BUILD -DDEBUG_DEVELOP"
)

# https://svn.boost.org/trac/boost/ticket/6455
set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS_DEBUG} -DQT_NO_KEYWORDS -DGDAL_LIBRARY_HAS_OGR_SUPPORT"
)



if(UNIX AND NOT CYGWIN)
    if(APPLE)
        set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
        set(CMAKE_INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib")
    else()
        set(CMAKE_SKIP_BUILD_RPATH FALSE)
        set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
        set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
        set(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)
    endif()
endif()


include(CheckCXXCompilerFlag)



# Related linking of fern lib to generated C++ code.
# set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--export-all-symbols")
# For executables, you can use:
# ADD_EXECUTABLE(NAME_OF_EXECUTABLE $ $)
# SET(LINK_FLAGS ${LINK_FLAGS} "-Wl,-whole-archive")
# TARGET_LINK_LIBRARIES(NAME_OF_EXECUTABLE ${PROJECT_NAME})




# TODO: Treat all warnings as errors and explicitly turn off warning that
#       we don't consider errors. Example(!):
# Clang:
# -Werror -Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic -Wno-exit-time-destructors -Wno-missing-braces -Wno-padded
# Visual Studio:
# /WX /Wall
# Gcc:
# -Werror -Wall -Wextra -Wpedantic -Wcast-align -Wcast-qual -Wconversion -Wctor-dtor-privacy -Wdisabled-optimization -Wdouble-promotion -Wfloat-equal -Wformat=2 -Winit-self -Winvalid-pch -Wlogical-op -Wmissing-declarations -Wmissing-include-dirs -Wnoexcept -Wold-style-cast -Woverloaded-virtual -Wredundant-decls -Wshadow -Wsign-conversion -Wsign-promo -Wstrict-null-sentinel -Wstrict-overflow=5 -Wtrampolines -Wundef -Wunsafe-loop-optimizations -Wvector-operation-performance -Wzero-as-null-pointer-constant


if(${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU" OR
        ${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
    # TODO Figure this out:
    # https://gcc.gnu.org/wiki/Visibility

    # The code assumes integer overflow and underflow wraps. This is not
    # guaranteed by the standard. Gcc may assume overflow/underflow will not
    # happen and optimize the code accordingly. That's why we added
    # -fno-strict-overflow. It would be better if we don't assume
    # over/underflow wraps.
    # See http://www.airs.com/blog/archives/120
    # See out of range policy of add algorithm for signed integrals.
    #
    # Add as many warning options as possible/useful:
    # - https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html
    # TODO Maybe add:
    # -Wconversion
    # -Wsign-conversion
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wcast-qual -Wwrite-strings -Werror=strict-aliasing -pedantic -fno-strict-overflow -ftemplate-backtrace-limit=0"
    )

    # Make linker report any unresolved symbols.
    set(CMAKE_SHARED_LINKER_FLAGS
        "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--no-undefined"
    )

    # This results in an error on mingw/gcc 4.8/windows. Some warning about
    # and unused parameters. Skip for now.
    # set(CMAKE_CXX_FLAGS_RELEASE
    #         "${CMAKE_CXX_FLAGS_RELEASE} -Werror"
    #     )

    # if(NOT MINGW)
    #     # This option triggers a warning on Windows, something in
    #     # boost.filesystem. Not fixing it now.
    #     set(CMAKE_CXX_FLAGS
    #         "${CMAKE_CXX_FLAGS} -Wzero-as-null-pointer-constant"
    #     )
    # endif()

    if(APPLE)
        set(CMAKE_CXX_FLAGS
            "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedefs"
        )
    endif()

    # TODO Revisit this option. Only needed for shared libraries.
    # Add the PIC compiler flag if needed.
    # See also CMake property POSITION_INDEPENDENT_CODE.
    if(UNIX AND NOT WIN32)
        if(CMAKE_SIZEOF_VOID_P MATCHES "8")
            CHECK_CXX_COMPILER_FLAG("-fPIC" WITH_FPIC)
            if(WITH_FPIC)
                add_definitions(-fPIC)
            endif()
        endif()
    endif()
endif()

if(${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
    if(${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
        set(CMAKE_CXX_FLAGS
            "${CMAKE_CXX_FLAGS} -std=c++1y"
        )
    else()
        set(CMAKE_CXX_FLAGS
            "${CMAKE_CXX_FLAGS} -std=c++14"
        )
    endif()
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -stdlib=libc++ -std=c++1y"
    )
endif()
