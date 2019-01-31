cmake_minimum_required(VERSION 3.7)

if(POLICY CMP0063)
  cmake_policy(SET CMP0063 NEW)
endif()

if(POLICY CMP0072)
  cmake_policy(SET CMP0072 NEW)
endif()

project(pcraster)

set(DEV_CURRENT_YEAR "2019")

set(${PROJECT_NAME}_ORGANISATION_NAME "PCRaster Research and Development Team")
set(${PROJECT_NAME}_COPYRIGHT
    "1990-${DEV_CURRENT_YEAR}, The ${${PROJECT_NAME}_ORGANISATION_NAME}")
set(${PROJECT_NAME}_PROJECT_NAME "${PROJECT_NAME}")
set(${PROJECT_NAME}_HOMEPAGE "http://www.pcraster.eu")
set(${PROJECT_NAME}_MAJOR_VERSION "4")
set(${PROJECT_NAME}_MINOR_VERSION "3")
set(${PROJECT_NAME}_PATCH_VERSION "0")
set(${PROJECT_NAME}_PRE_RELEASE_VERSION "dev")
set(${PROJECT_NAME}_VERSION
    "${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}-${${PROJECT_NAME}_PRE_RELEASE_VERSION}")

set(CMAKE_MODULE_PATH
    $ENV{CMAKE_MODULE_PATH}
    ${CMAKE_CURRENT_SOURCE_DIR}/devbase/cmake
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


install(
  FILES
    ${CMAKE_CURRENT_BINARY_DIR}/pcraster_version.txt
  DESTINATION
    doc
)

# Display configuration summary
message(STATUS "-------------------------------------------------")
message(STATUS "PCRaster configuration summary:")
message(STATUS "  Version:                " ${${PROJECT_NAME}_VERSION})
message(STATUS "  Installation directory: " ${CMAKE_INSTALL_PREFIX} )
message(STATUS "  Build options:")
message(STATUS "    Type:                 " ${CMAKE_BUILD_TYPE})
message(STATUS "    Aguila:               " ${PCRASTER_BUILD_AGUILA})
message(STATUS "    Multicore:            " ${PCRASTER_BUILD_MULTICORE})
message(STATUS "    Modflow:              " ${PCRASTER_BUILD_MODFLOW})
message(STATUS "    Oldcalc:              " ${PCRASTER_BUILD_OLDCALC})
message(STATUS "    Tests:                " ${PCRASTER_BUILD_TEST})
message(STATUS "    Documentation:        " ${PCRASTER_BUILD_DOCUMENTATION})
message(STATUS "  Experimental:")
message(STATUS "    BlockPython:          " ${PCRASTER_BUILD_BLOCKPYTHON})
message(STATUS "    Moc:                  " ${PCRASTER_BUILD_MOC})
message(STATUS "    Mldd:                 " ${PCRASTER_BUILD_MLDD})
message(STATUS "-------------------------------------------------")
