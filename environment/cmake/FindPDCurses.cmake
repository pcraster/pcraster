# The original version of this file can be found at
# https://github.com/aevalo/aeirc/blob/master/cmake/Modules/FindPDCurses.cmake

# Locate PDCurses library
# This module defines
# PDCURSES_LIBRARIES, the name of the library to link against
# PDCURSES_FOUND, if false, do not try to link to PDCurses
# PDCURSES_INCLUDE_DIR, where to find curses.h

FIND_PATH(PDCURSES_INCLUDE_DIR curses.h
  HINTS
  $ENV{PDCURSESDIR}
  PATH_SUFFIXES include/pdcurses include)

FIND_LIBRARY(PDCURSES_LIBRARY
  NAMES pdcurses
  HINTS
  $ENV{PDCURSESDIR}
  PATH_SUFFIXES lib64 lib)

FIND_LIBRARY(PDCURSES_PANEL_LIBRARY
  NAMES panel
  HINTS
  $ENV{PDCURSESDIR}
  PATH_SUFFIXES lib64 lib)

IF(PDCURSES_LIBRARY)
  SET(PDCURSES_LIBRARIES ${PDCURSES_LIBRARY})
  IF(PDCURSES_PANEL_LIBRARY)
    SET(PDCURSES_LIBRARIES ${PDCURSES_LIBRARIES} ${PDCURSES_PANEL_LIBRARY})
  ENDIF(PDCURSES_PANEL_LIBRARY)
ENDIF(PDCURSES_LIBRARY)

SET(PDCURSES_FOUND "NO")
IF(PDCURSES_INCLUDE_DIR AND PDCURSES_LIBRARY)
  SET(PDCURSES_FOUND "YES")
  message(STATUS "Found PDCurses:")
  message(STATUS "  includes : ${PDCURSES_INCLUDE_DIR}")
  message(STATUS "  libraries: ${PDCURSES_LIBRARIES}")

  # Set the final string here so the GUI reflects the final state.
  # SET(PDCURSES_LIBRARIES PDCURSES_LIBRARY} CACHE STRING "Where the PDCurses Library can be found")
ENDIF(PDCURSES_INCLUDE_DIR AND PDCURSES_LIBRARY)
