set(AGUILA_MAJOR_VERSION ${${PROJECT_NAME}_MAJOR_VERSION})
set(AGUILA_MINOR_VERSION ${${PROJECT_NAME}_MINOR_VERSION})
set(AGUILA_PATCH_VERSION ${${PROJECT_NAME}_PATCH_VERSION})
set(AGUILA_PRE_RELEASE_VERSION ${${PROJECT_NAME}_PRE_RELEASE_VERSION})

set(AGUILA_VERSION
    "${AGUILA_MAJOR_VERSION}.${AGUILA_MINOR_VERSION}.${AGUILA_PATCH_VERSION}-${AGUILA_PRE_RELEASE_VERSION}")

set(CMAKE_AUTOMOC ON)

configure_file(
    ag_Configure.h.in
    ../include/ag_Configure.h
)

qt5_wrap_ui(
    UI_SOURCES
    # CursorValueDialog.ui
    GeneralPreferencesWidgetBase.ui
    PreferencesDialogBase.ui
    SaveDataAsDialogBase.ui
    SaveViewAsDialogBase.ui
)

if(PCRASTER_WITH_OPENGL)
    add_library(pcraster_aguila_opengl_objects OBJECT
        ag_Camera
        ag_Cube
        ag_Feedback
        ag_GLVisualisation
        ag_Map3D
        ag_Map3DObject
        ag_Map3DView
        ag_Map3DWindow
        ag_Scene
        ag_SceneObject
        ag_SceneView
        ag_PostScript
        ag_QuadDrawer
        ag_Quaternion
    )
    target_compile_definitions(pcraster_aguila_opengl_objects
        PRIVATE
            PCR_DAL_SHARED_LINK
            PCR_AG_SOURCE
            PCR_AG_SHARED_LINK
    )
    target_include_directories(pcraster_aguila_opengl_objects
        PRIVATE
            ${Qt5OpenGL_INCLUDE_DIRS}
            $<TARGET_PROPERTY:pcraster::math_utils,INTERFACE_INCLUDE_DIRECTORIES>
            $<TARGET_PROPERTY:pcraster::dal,INTERFACE_INCLUDE_DIRECTORIES>
    )
endif()

add_library(pcraster_aguila_objects OBJECT
    ag_Aguila
    ag_AguilaProgramOptions
    ag_AnimationControl
    ag_BooleanDrawProps
    ag_BooleanRasterDrawer
    ag_BufferedVisualisation
    ag_BufferedWidget
    ag_ClassDrawPropertiesWidget
    ag_ClassDrawProps
    ag_ClassLegendBody
    ag_ColourSelector
    ag_Configurable
    ag_CumDistributionFunction
    ag_CumDistributionFunctionView
    ag_CumDistributionFunctionWindow
    ag_CursorView
    ag_CursorWindow
    ag_DataConfiguration
    ag_DataGuide
    ag_DataInfo
    ag_DataManager
    ag_DataObjectBase
    ag_DataObject
    ag_DataProperties
    ag_DataPropertiesDialog
    ag_DataProperty
    ag_Dataset
    ag_DataSourceTable
    ag_DimensionCoordinateEdit
    ag_DirectionalRasterDrawer
    ag_DOManager
    ag_DrawPropertiesWidget
    ag_DrawProps
    ExceedanceProbabilityFeatureLayerDrawer
    ag_ExceedanceProbabilityRasterDrawer
    ag_FeatureDataSources
    ag_FeatureLayer
    ag_FeatureLayerDrawer
    ag_GeneralDataPropertiesWidget
    ag_GeneralPreferencesWidget
    ag_GeometryLegendBody
    ag_IVisualisation
    ag_LddDrawProps
    ag_LddRasterDrawer
    ag_LegendBody
    ag_Legend
    ag_LegendView
    ag_LineMarker
    ag_Map2D
    ag_Map2DView
    ag_Map2DWindow
    ag_Map
    ag_MapDrawer
    ag_MapWindow
    ag_MouseTarget
    ag_MultiMap2D
    ag_MultiMap2DView
    ag_MultiMap2DWindow
    ag_NominalDrawProps
    ag_NominalRasterDrawer
    ag_OrdinalDrawProps
    ag_OrdinalRasterDrawer
    ag_PlotVisualisation
    ag_PlotView
    ag_PreferencesDialog
    ag_PropertiesDialog
    ag_PropertiesWidget
    ag_QApplication
    ag_RangeDrawPropertiesWidget
    ag_RangeDrawProps
    ag_RangeFeatureLayerDrawer
    ag_RangeLegendBody
    ag_Raster
    ag_RasterDataset
    ag_RasterDataSources
    ag_RasterDrawer
    ag_SaveDataAsDialog
    ag_SaveViewAsDialog
    ag_ScalarRasterDrawer
    ag_SpatialDataset
    ag_Table
    ag_TableDataSources
    ag_TableVisualisation
    ag_Templates
    ag_Time
    ag_TimePlot
    ag_TimePlotWindow
    ag_Util
    ag_Vector
    ag_VectorDataSources
    ag_VectorDrawer
    ag_Viewer
    ag_VisChangeManager
    ag_VisEngine
    ag_VisGroup
    ag_VisGroupManager
    ag_VisObserver
    ag_VisSubject
    ag_Visualisation
    ag_VisualisationDialog
    ag_VisualisationWindow
    ag_XMLViewItems

    com_classifier
    com_colour
    com_const
    com_exception
    com_fileformatinfo
    com_palette
    com_rangemap
    com_rawpalette
    com_rcobject
    com_rcsize_t
    com_rgbtuple
    com_util

    geo_dataguide
    geo_DataType

    qt_Animation
    qt_AnimationProgBar
    qt_AppWindow
    qt_AppWindowProperties
    qt_ColourLib
    qt_Dialog
    qt_EventFilterSubject
    qt_GuiApp
    qt_New
    qt_PropertiesDialog
    qt_PropertiesWidget
    # qt_SelectableWidget
    qt_Util

    qtd_SelectPalette

    qtw_PaletteBar

    # Plots/ag_ProbabilityPlot
    # Plots/ag_ProbabilityTimeSeriesPlot
    # Plots/ag_ProbabilityValuePlot
    # Plots/ag_TimeSeriesPlot
    # Plots/ag_ValueTimeSeriesPlot

    ${UI_SOURCES}

)

target_compile_definitions(pcraster_aguila_objects
    PRIVATE
        PCR_DAL_SHARED_LINK
)


target_include_directories(pcraster_aguila_objects
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        ${Qt5Gui_INCLUDE_DIRS}
        ${Qt5Xml_INCLUDE_DIRS}
        ${Qt5OpenGL_INCLUDE_DIRS}
        ${Qt5Widgets_INCLUDE_DIRS}
        $<TARGET_PROPERTY:pcraster::math_utils,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::string_utils,INTERFACE_INCLUDE_DIRECTORIES>
)

target_include_directories(pcraster_aguila_objects
    PUBLIC
        $<TARGET_PROPERTY:pcraster::dal,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::raster_format,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::utils,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::xsd,INTERFACE_INCLUDE_DIRECTORIES>
)

# add_dependencies(pcraster_aguila_objects
#     # Some of the sources include PCRasterXSD.h, which is generated by
#     # pcraster::xsd. Since we don't link the objects library against
#     # pcraster::xsd, but only use the include directories, CMake doesn't
#     # make pcraster_aguila_objects depend on pcraster::xsd. We need
#     # to do that ourselves. If not, PCRasterXSD.h may not be there during
#     # the build of the objects in the objects library.
#     pcraster::xsd
# )

# Is the above obsolete when using target_link_libraries?
target_link_libraries(pcraster_aguila_objects
  pcraster::xsd
)

target_compile_definitions(pcraster_aguila_objects
    PRIVATE
        PCR_AG_SOURCE
        PCR_AG_SHARED_LINK
)


if(PCRASTER_WITH_OPENGL)
  target_compile_definitions(pcraster_aguila_objects
    PRIVATE
      AGUILA_WITH_OPENGL
  )
endif()

if(PCRASTER_WITH_OPENGL)
    add_library(pcraster_aguila SHARED
        $<TARGET_OBJECTS:pcraster_aguila_opengl_objects>
        $<TARGET_OBJECTS:pcraster_aguila_objects>
    )
else()
    add_library(pcraster_aguila SHARED
        $<TARGET_OBJECTS:pcraster_aguila_objects>
    )
endif()

add_library(pcraster::aguila ALIAS pcraster_aguila)

set_target_properties(pcraster_aguila
    PROPERTIES
        SOVERSION ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}
)

if(PCRASTER_WITH_OPENGL)
    target_link_libraries(pcraster_aguila
        PUBLIC
            pcraster::dal
            pcraster::xsd
            Qt5::Gui
            Qt5::Widgets
            Qt5::Xml
            Qt5::Charts
            Qt5::OpenGL
            OpenGL::GLU
    )
else()
    target_link_libraries(pcraster_aguila
        PUBLIC
            pcraster::dal
            pcraster::xsd
            Qt5::Gui
            Qt5::Widgets
            Qt5::Xml
            Qt5::Charts
    )
endif()


install(
    TARGETS pcraster_aguila
    RUNTIME DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
    LIBRARY DESTINATION ${PCRASTER_LIB_INSTALL_DIR} COMPONENT Runtime
)

add_executable(aguila
    WIN32 # Use WinMain instead of main. Gets rid of Dos box.
    main
)

target_compile_definitions(aguila
    PRIVATE
        PCR_AG_SHARED_LINK
)

target_link_libraries(aguila
    pcraster::aguila
    # ${QT_QTMAIN_LIBRARY}  # Contains WinMain.
)

install(
    TARGETS aguila
    RUNTIME DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
)

# ------------------------------------------------------------------------------

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )
    set(AGUILA_TEST_SOURCES            # ag_AguilaTest  # Requires an X-server.
        ag_AguilaProgramOptionsTest
        ag_ClassifierTest
        ag_ColourSelectorTest
        DataObjectBaseTest
        DataManagerTest
        com_fileformatinfotest
        com_matrixtest
        com_rangemaptest
        com_rgbtupletest
        com_utiltest
        com_userdefinedclassifiertest
        com_vectortest
        # SUPPORT_NAMES ag_AguilaGuiTest  # Only needed for AguilaTest.
    )
    set_property(
        SOURCE ${AGUILA_TEST_SOURCES}
        PROPERTY COMPILE_DEFINITIONS
            # PCR_AG_SOURCE
            PCR_AG_SHARED_LINK
    )
    if(PCRASTER_WITH_OPENGL)
        add_unit_tests(
            SCOPE pcraster_aguila
            NAMES
                ${AGUILA_TEST_SOURCES}
            # LINK_LIBRARIES
                # pcraster_aguila_static
                # ${_target_libraries}
                # #${QT_QTTEST_LIBRARY}
            INCLUDE_DIRS
                $<TARGET_PROPERTY:pcraster_aguila_objects,INTERFACE_INCLUDE_DIRECTORIES>
            OBJECT_LIBRARIES
                $<TARGET_OBJECTS:pcraster_aguila_objects>
                $<TARGET_OBJECTS:pcraster_aguila_opengl_objects>
            LINK_LIBRARIES
                $<TARGET_PROPERTY:pcraster_aguila,LINK_LIBRARIES>
        )
    else()
        add_unit_tests(
            SCOPE pcraster_aguila
            NAMES
                ${AGUILA_TEST_SOURCES}
            # LINK_LIBRARIES
                # pcraster_aguila_static
                # ${_target_libraries}
                # #${QT_QTTEST_LIBRARY}
            INCLUDE_DIRS
                $<TARGET_PROPERTY:pcraster_aguila_objects,INTERFACE_INCLUDE_DIRECTORIES>
            OBJECT_LIBRARIES
                $<TARGET_OBJECTS:pcraster_aguila_objects>
            LINK_LIBRARIES
                $<TARGET_PROPERTY:pcraster_aguila,LINK_LIBRARIES>
        )
    endif()
    add_test(
        NAME setup_pcraster_aguila
        COMMAND ${Python3_EXECUTABLE} test_prolog.py
    )
    set_tests_properties(setup_pcraster_aguila
        PROPERTIES
            FIXTURES_SETUP
                FixtureAguila
    )
    set_tests_properties(pcraster_aguila_ag_AguilaProgramOptionsTest
        PROPERTIES
            FIXTURES_REQUIRED
                FixtureAguila
    )
endif()
