# DLexerBase.o ???
add_library(pccts STATIC
    AParser.cpp
    ATokenBuffer.cpp
)

add_library(pcraster::pccts ALIAS pccts)

target_include_directories(pccts
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)
