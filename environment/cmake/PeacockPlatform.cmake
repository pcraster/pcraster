# This script determines the characteristics of the host and target platforms
# and normalizes the names. The resulting variables can be used in the package
# build scripts and in configuring external projects.
# At the bottom is a list of the variables set by this script.
# This script depends on TargetArch script, so when using it to configure
# external projects, make sure to checkout that script as well.
# See the README.md for some more information, including the list of
# normalized names.

set(host_system_name ${CMAKE_HOST_SYSTEM_NAME})
set(target_system_name ${CMAKE_SYSTEM_NAME})
set(compiler_id ${CMAKE_CXX_COMPILER_ID})
set(compiler_version ${CMAKE_CXX_COMPILER_VERSION})


# Determine target architecture.
# https://github.com/petroules/solar-cmake/blob/master/TargetArch.cmake
include(TargetArch)
target_architecture(target_architecture)


# Normalize target architecture:
# x86_32, x86_64
if(${target_architecture} STREQUAL "i386")
    set(target_architecture "x86_32")
elseif(${target_architecture} STREQUAL "AMD64")
    set(target_architecture "x86_64")
endif()


# Normalize host system name:
# linux, windows, cygwin
if(${host_system_name} STREQUAL "Linux")
    set(host_system_name "linux")
elseif(${host_system_name} STREQUAL "Darwin")
    set(host_system_name "darwin")
elseif(${host_system_name} STREQUAL "Windows")
    if(CYGWIN)
        set(host_system_name "cygwin")
    else()
        set(host_system_name "windows")
    endif()
endif()


# Normalize target system name:
# linux, windows, cygwin
if(${target_system_name} STREQUAL "Linux")
    set(target_system_name "linux")
elseif(${target_system_name} STREQUAL "Darwin")
    set(target_system_name "darwin")
elseif(${target_system_name} STREQUAL "Windows")
    if(CYGWIN)
        set(target_system_name "cygwin")
    else()
        set(target_system_name "windows")
    endif()
endif()


# Normalize compiler id:
# clang, gcc, mingw, msvc
if(${compiler_id} STREQUAL "GNU")
    if(MINGW)
        set(compiler_id "mingw")
    else()
        set(compiler_id "gcc")
    endif()
elseif(${compiler_id} STREQUAL "MSVC")
    set(compiler_id "msvc")
elseif(${compiler_id} STREQUAL "Clang")
    set(compiler_id "clang")
endif()


if((${compiler_id} STREQUAL "gcc") OR (${compiler_id} STREQUAL "mingw") OR
        (${compiler_id} STREQUAL "clang"))
    string(FIND ${compiler_version} "." period_index)
    string(SUBSTRING ${compiler_version} 0 ${period_index}
        compiler_main_version)
elseif(${compiler_id} STREQUAL "msvc")
    if(MSVC12)
        # 18.0.21005.1
        set(compiler_version "12")
        set(compiler_main_version "12")
    endif()
endif()


# <host>/<target>/<compiler>-<version>/<architecture>
set(peacock_target_platform ${host_system_name}/${target_system_name}/${compiler_id}-${compiler_main_version}/${target_architecture})


if(${host_system_name} STREQUAL ${target_system_name})
    set(peacock_cross_compiling FALSE)
else()
    set(peacock_cross_compiling TRUE)
endif()


# Set a variable to the host specification as used by some build scripts.
# In Gcc's terms, host is the same as what is called target here. It is the
# machine running the software. In Peacock's terminology, host is the machine
# building the software. Sigh...
# https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html
#
# +---------+---------+--------+
# | Tool    | Builder | Runner |
# +---------+---------+--------+
# | Peacock | host    | target |
# | Gcc     | build   | host   |
# +---------+---------+--------+

# See also:
# - http://www.cmake.org/cmake/help/v3.0/module/GNUInstallDirs.html
# - https://wiki.debian.org/Multiarch


if(${target_architecture} STREQUAL "x86_32")
    if(${target_system_name} STREQUAL "linux")
        message(FATAL_ERROR Add "configure host")
    elseif(${target_system_name} STREQUAL "windows")
        if(${compiler_id} STREQUAL "mingw")
            set(peacock_gnu_configure_host "i686-w64-mingw32")
        else()
            message(FATAL_ERROR "Add GNU configure host")
        endif()
    elseif(${target_system_name} STREQUAL "cygwin")
        message(FATAL_ERROR "Add GNU configure host")
    else()
        message(FATAL_ERROR "Add GNU configure host")
    endif()
elseif(${target_architecture} STREQUAL "x86_64")
    if(${target_system_name} STREQUAL "linux")
        if(${compiler_id} STREQUAL "gcc")
            set(peacock_gnu_configure_host "x86_64-unknown-linux")
        elseif(${compiler_id} STREQUAL "clang")
            set(peacock_gnu_configure_host "x86_64-unknown-linux")
        else()
            message(FATAL_ERROR "Add GNU configure host")
        endif()
    elseif(${target_system_name} STREQUAL "windows")
        if(${compiler_id} STREQUAL "mingw")
            set(peacock_gnu_configure_host "x86_64-w64-mingw32")
        else()
            message(FATAL_ERROR "Add GNU configure host")
        endif()
    elseif(${target_system_name} STREQUAL "cygwin")
        message(FATAL_ERROR Add "configure host spec")
    else()
        message(FATAL_ERROR "Add GNU configure host for system/target: "
            "${target_system_name}/${target_architecture}")
    endif()
endif()



# TODO build_spec:
# - i686-pc-cygwin


message(STATUS "peacock: cross_compiling      : " ${peacock_cross_compiling})
message(STATUS "peacock: host_system_name     : " ${host_system_name})
message(STATUS "peacock: target_system_name   : " ${target_system_name})
message(STATUS "peacock: target_architecture  : " ${target_architecture})
message(STATUS "peacock: compiler_id          : " ${compiler_id})
message(STATUS "peacock: compiler_version     : " ${compiler_version})
message(STATUS "peacock: compiler_main_version: " ${compiler_main_version})
message(STATUS "peacock: target_platform      : " ${peacock_target_platform})
message(STATUS "peacock: gnu_configure_host   : " ${peacock_gnu_configure_host})
