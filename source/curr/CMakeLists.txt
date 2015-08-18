include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
)

SET(SOURCES
    currmenu.c
    keyfunc.c
)
ADD_LIBRARY(curr
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(curr
    misc
)
