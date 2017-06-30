# CMAKE_CFG_INTDIR is set at build-time, not at configure-time. Search
# and replace this string at build-time.
# Required to pick up the correct binaries while generating the
# unit test files

# AscHeader2map
FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/AscHeader2map.py FCONTENT)
STRING(REPLACE "asc2map" ${CMAKE_ARGV3} FCONTENT ${FCONTENT})
FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/AscHeader2map.py "${FCONTENT}")

# PCRasterSample.py
FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py FCONTENT)
STRING(REPLACE "REPLACE_EXECUTABLE_PATH" ${CMAKE_ARGV4} FCONTENT ${FCONTENT})
FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py "${FCONTENT}")
