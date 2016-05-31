# utilities to obtain the version information of the current build
# ${CMAKE_CURRENT_SOURCE_DIR} is not pointing to the correct directory...

find_package(Git)

execute_process(
  WORKING_DIRECTORY
    "${SRC_DIR}"
  COMMAND
    ${GIT_EXECUTABLE} rev-parse --short HEAD
    OUTPUT_VARIABLE GIT_REV_SHORT
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  WORKING_DIRECTORY
    "${SRC_DIR}"
  COMMAND
    ${GIT_EXECUTABLE} rev-parse HEAD
    OUTPUT_VARIABLE GIT_REV_LONG
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  WORKING_DIRECTORY
    "${SRC_DIR}"
  COMMAND
    ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
    OUTPUT_VARIABLE GIT_REV_BRANCH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

string(TIMESTAMP _curr_date "%d.%m.%Y")

file(
  WRITE
    ${CMAKE_CURRENT_BINARY_DIR}/pcraster_version.txt
"PCRaster version  ${VER}
built on ${_curr_date}
using branch '${GIT_REV_BRANCH}' with commit '${GIT_REV_SHORT}' (${GIT_REV_LONG})
"
)
