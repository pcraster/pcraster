cmake_minimum_required(VERSION 3.22)

# Policies for PCRaster and subprojects
if(POLICY CMP0177)
    cmake_policy(SET CMP0177 NEW)
    set(CMAKE_POLICY_DEFAULT_CMP0177 NEW)
endif()

if(POLICY CMP0167)
    cmake_policy(SET CMP0167 NEW)
    set(CMAKE_POLICY_DEFAULT_CMP0167 NEW)
endif()

if(POLICY CMP0156)
    cmake_policy(SET CMP0156 NEW)
    set(CMAKE_POLICY_DEFAULT_CMP0156 NEW)
endif()

if(POLICY CMP0144)
    cmake_policy(SET CMP0144 NEW)
    set(CMAKE_POLICY_DEFAULT_CMP0144 NEW)
endif()

project(pcraster)

set(DEV_CURRENT_YEAR "2025")

set(${PROJECT_NAME}_ORGANISATION_NAME "PCRaster owners")
set(${PROJECT_NAME}_COPYRIGHT
    "1990-${DEV_CURRENT_YEAR}, ${${PROJECT_NAME}_ORGANISATION_NAME}")
set(${PROJECT_NAME}_PROJECT_NAME "${PROJECT_NAME}")
set(${PROJECT_NAME}_HOMEPAGE "http://www.pcraster.eu")
set(${PROJECT_NAME}_MAJOR_VERSION "4")
set(${PROJECT_NAME}_MINOR_VERSION "5")
set(${PROJECT_NAME}_PATCH_VERSION "0")
set(${PROJECT_NAME}_PRE_RELEASE_VERSION "dev")
set(${PROJECT_NAME}_VERSION
    "${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}")
if(NOT ${${PROJECT_NAME}_PRE_RELEASE_VERSION} STREQUAL "")
  string(APPEND ${PROJECT_NAME}_VERSION "-${${PROJECT_NAME}_PRE_RELEASE_VERSION}")
endif()

set(CMAKE_MODULE_PATH
    $ENV{CMAKE_MODULE_PATH}
    ${CMAKE_CURRENT_SOURCE_DIR}/environment/cmake
)

include(PCRaster)

add_subdirectory(source)
add_subdirectory(documentation)

# Create version information file
# Dummy target that does nothing. This enables us to attach build commands.
add_custom_target(version_info_dummy_target ALL)

add_custom_command(
    TARGET
      version_info_dummy_target
    PRE_BUILD
    COMMAND
      "${CMAKE_COMMAND}" -DVER=${pcraster_VERSION} -DSRC_DIR=${CMAKE_CURRENT_SOURCE_DIR} -P "${CMAKE_CURRENT_SOURCE_DIR}/environment/cmake/PCRasterVersionInformation.cmake"
)

if(PCRASTER_INSTALL_FILES_DEVELOPER)
    install(
      FILES
        ${CMAKE_CURRENT_BINARY_DIR}/pcraster_version.txt
      DESTINATION
        doc
    )
endif()

# Display configuration summary
message(STATUS "-----------------------------------------------------------")
message(STATUS "PCRaster configuration summary:")
message(STATUS "  Version:                  " ${${PROJECT_NAME}_VERSION} " (" ${CMAKE_HOST_SYSTEM_PROCESSOR} ")")
message(STATUS "  Directories:")
message(STATUS "    Installation:")
message(STATUS "      Binaries:             " ${CMAKE_INSTALL_PREFIX}/${PCRASTER_BIN_INSTALL_DIR} )
message(STATUS "      Libraries:            " ${CMAKE_INSTALL_PREFIX}/${PCRASTER_LIB_INSTALL_DIR} )
message(STATUS "      Python:               " ${CMAKE_INSTALL_PREFIX}/${PCRASTER_PYTHON_INSTALL_DIR} )
message(STATUS "    Building:")
message(STATUS "      Sources:              " ${CMAKE_SOURCE_DIR})
message(STATUS "      Path:                 " ${CMAKE_BINARY_DIR})
message(STATUS "  Build options:")
message(STATUS "    Aguila:                 " ${PCRASTER_BUILD_AGUILA})
message(STATUS "      OpenGL support:       " ${PCRASTER_WITH_OPENGL})
message(STATUS "    Multicore:              " ${PCRASTER_BUILD_MULTICORE})
message(STATUS "    Modflow:                " ${PCRASTER_BUILD_MODFLOW})
message(STATUS "      Build mf2005:         " ${PCRASTER_BUILD_MODFLOW_EXECUTABLE})
message(STATUS "    Oldcalc:                " ${PCRASTER_BUILD_OLDCALC})
message(STATUS "    Tests:                  " ${PCRASTER_BUILD_TEST})
message(STATUS "    Documentation:          " ${PCRASTER_BUILD_DOCUMENTATION})
message(STATUS "  Experimental features:")
message(STATUS "    BlockPython:            " ${PCRASTER_BUILD_BLOCKPYTHON})
message(STATUS "    Moc:                    " ${PCRASTER_BUILD_MOC})
message(STATUS "    Mldd:                   " ${PCRASTER_BUILD_MLDD})
message(STATUS "  Build configuration:")
message(STATUS "    Generator:              " ${CMAKE_GENERATOR})
message(STATUS "    Type(s):                " ${CMAKE_BUILD_TYPE} "${CMAKE_CONFIGURATION_TYPES}")
message(STATUS "    Flags:                  ")
message(STATUS "      Native:               " ${PCRASTER_WITH_FLAGS_NATIVE})
message(STATUS "      IPO:                  " ${PCRASTER_WITH_FLAGS_IPO})
message(STATUS "-----------------------------------------------------------")

# Only configured for packaging source code required to build a package!
# Some parts are excluded (e.g docs, 32-bit binaries, ...) to reduce file size.
include(PCRasterPackaging)
