# This module defines
# XSD_INCLUDE_DIRS, where to find elements.hxx, etc.
# XSD_EXECUTABLE, where is the xsd compiler

find_program(XSD_EXECUTABLE
    NAMES xsd xsdcxx
    PATHS
        "[HKEY_CURRENT_USER\\xsd\\bin]"
        $ENV{XSDDIR}/bin
        "C:/Program Files/CodeSynthesis XSD 3.2/bin"
        "D:/Program Files/CodeSynthesis XSD 3.2/bin"
        "C:/Program Files/CodeSynthesis XSD 3.3/bin"
        "C:/Program Files (x86)/CodeSynthesis XSD 3.3/bin"
)


if(XSD_EXECUTABLE)
    get_filename_component(XSD_ROOT_PATHNAME ${XSD_EXECUTABLE} DIRECTORY)
    set(_XSD_ROOT_PATHNAME ${XSD_ROOT_PATHNAME}/..)

    find_path(XSD_INCLUDE_DIRS xsd/cxx/parser/elements.hxx
        ${_XSD_ROOT_PATHNAME}/include
        ${_XSD_ROOT_PATHNAME}/libxsd
        "[HKEY_CURRENT_USER\\software\\xsd\\include]"
        "[HKEY_CURRENT_USER]\\xsd\\include]"
        $ENV{XSDDIR}/include
        /usr/local/include
        /usr/include
        "C:/Program Files/CodeSynthesis XSD 3.2/include"
        "D:/Program Files/CodeSynthesis XSD 3.2/include"
        "C:/Program Files/CodeSynthesis XSD 3.3/include"
        "C:/Program Files (x86)/CodeSynthesis XSD 3.3/include"
        ${CMAKE_SOURCE_DIR}/../xsd/libxsd
    )
endif()


if(NOT XSD_INCLUDE_DIRS)
    message (FATAL_ERROR
        "Unable to find xsd include files (xsd/cxx/parser/elements.hxx)")
endif()

if(NOT XSD_EXECUTABLE)
    message (FATAL_ERROR "Unable to find xsd or xsdcxx executable")
endif()


if(XSD_INCLUDE_DIRS AND XSD_EXECUTABLE)
    set(XSD_FOUND TRUE)
endif()


mark_as_advanced(
    XSD_INCLUDE_DIRS
    XSD_EXECUTABLE
)
