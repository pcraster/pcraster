set(SPHINX_SOURCES
    AguilaOnMac.rst
    Building.rst
    Data.rst
    Dialogs.rst
    Downloads.rst
    Enhancements.rst
    Faq.rst
    History.rst
    index.rst
    Installation.rst
    Introduction.rst
    License.rst
    Links.rst
    Manual.rst
    MultiMap2DView.rst
    Publications.rst
    QuickStart.rst
    Screenshots.rst
    Support.rst
    UncertainSpatioTemporal.rst
    Vector.rst
    Views.rst
    XmlStartupConfiguration.rst
)

create_sphinx_docs(
    TARGET pcraster_aguila_documentation
    SOURCES ${SPHINX_SOURCES}
)

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_build/html/
    DESTINATION doc/aguila
)
