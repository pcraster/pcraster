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
    Images/AguilaOnMac.png
    Images/AguilaVideo.png
    Images/AnimationDialog.png
    Images/CursorAndValuesView.png
    Images/dem_Map.png
    Images/dem+soil_Drape.png
    Images/krige_ProbabilityGraph.png
    Images/MultiMap2DView.png
    Images/rain_TimeGraph.png
    Images/UncertainSpatioTemporal.png
    Images/Vector1.png
    Images/Vector2.png
    Videos/2009-10-08_2222.swf
)

prepare_pcraster_project_docs("${SPHINX_SOURCES}" "pcraster_aguila" "AGUILA_SPHINX_SOURCES")

create_sphinx_docs(
    TARGET pcraster_aguila_documentation
    SOURCES ${SPHINX_SOURCES}
)

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_build/html/
    DESTINATION doc/aguila
)
