add_subdirectory(examples)

if(PCRASTER_BUILD_DOCUMENTATION)
    add_subdirectory(figures)
    add_subdirectory(sphinx)
endif()

set(PCRASTER_SPHINX_SOURCES ${PCRASTER_SPHINX_SOURCES} PARENT_SCOPE)
