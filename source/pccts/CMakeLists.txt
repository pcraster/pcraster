SET(SOURCES
    AParser.cpp
    ATokenBuffer.cpp
)
# DLexerBase.o ???
ADD_LIBRARY(pccts
    ${SOURCES}
)
