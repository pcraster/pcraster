# CMAKE_CFG_INTDIR is set at build-time, not at configure-time. Search
# and replace this string at build-time.
# Required to pick up the correct binaries while generating the
# unit test files

# AscHeader2map
FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/AscHeader2map.py FCONTENT1)
STRING(REPLACE "REPLACE_EXECUTABLE_PATH" "${CMAKE_ARGV3}" FCONTENT1 "${FCONTENT1}")
FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/AscHeader2map.py "${FCONTENT1}")

# PCRasterSample.py
FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py FCONTENT2)
STRING(REPLACE "REPLACE_EXECUTABLE_PATH" "${CMAKE_ARGV4}" FCONTENT2 "${FCONTENT2}")
FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/PCRasterSample.py "${FCONTENT2}")

