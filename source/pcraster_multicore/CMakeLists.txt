# OLS: check if this is safe to apply to the source directory instead
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/../
)


add_subdirectory(python)

if(PCRASTER_BUILD_TEST)
    add_subdirectory(tests)
endif()
