set(AGUILA_MAJOR_VERSION ${${PROJECT_NAME}_MAJOR_VERSION})
set(AGUILA_MINOR_VERSION ${${PROJECT_NAME}_MINOR_VERSION})
set(AGUILA_PATCH_VERSION ${${PROJECT_NAME}_PATCH_VERSION})
set(AGUILA_PRE_RELEASE_VERSION ${${PROJECT_NAME}_PRE_RELEASE_VERSION})

set(AGUILA_VERSION
    "${AGUILA_MAJOR_VERSION}.${AGUILA_MINOR_VERSION}.${AGUILA_PATCH_VERSION}-${AGUILA_PRE_RELEASE_VERSION}")

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

configure_file(
    ag_Configure.h.in
    ../include/ag_Configure.h
)
if(PCRASTER_WITH_OPENGL)
    set(AGUILA_OPENGL_SOURCES
        ag_Camera.cc
        ag_Cube.cc
        ag_Feedback.cc
        ag_GLVisualisation.cc
        ag_Map3D.cc
        ag_Map3DObject.cc
        ag_Map3DView.cc
        ag_Map3DWindow.cc
        ag_Scene.cc
        ag_SceneObject.cc
        ag_SceneView.cc
        ag_PostScript.cc
        ag_QuadDrawer.cc
        ag_Quaternion.cc
    )
endif()

add_library(pcraster_aguila_objects OBJECT
    # CursorValueDialog.ui
    GeneralPreferencesWidgetBase.ui
    PreferencesDialogBase.ui
    SaveDataAsDialogBase.ui
    SaveViewAsDialogBase.ui

    ag_Aguila.cc
    ag_AguilaProgramOptions.cc
    ag_AnimationControl.cc
    ag_BooleanDrawProps.cc
    ag_BooleanRasterDrawer.cc
    ag_BufferedVisualisation.cc
    ag_BufferedWidget.cc
    ag_ClassDrawPropertiesWidget.cc
    ag_ClassDrawProps.cc
    ag_ClassLegendBody.cc
    ag_ColourSelector.cc
    ag_Configurable.cc
    ag_CumDistributionFunction.cc
    ag_CumDistributionFunctionView.cc
    ag_CumDistributionFunctionWindow.cc
    ag_CursorView.cc
    ag_CursorWindow.cc
    ag_DataConfiguration.cc
    ag_DataGuide.cc
    ag_DataInfo.cc
    ag_DataManager.cc
    ag_DataObjectBase.cc
    ag_DataObject.cc
    ag_DataProperties.cc
    ag_DataPropertiesDialog.cc
    ag_DataProperty.cc
    ag_Dataset.cc
    ag_DataSourceTable.cc
    ag_DimensionCoordinateEdit.cc
    ag_DirectionalRasterDrawer.cc
    ag_DOManager.cc
    ag_DrawPropertiesWidget.cc
    ag_DrawProps.cc
    ExceedanceProbabilityFeatureLayerDrawer.cc
    ag_ExceedanceProbabilityRasterDrawer.cc
    ag_FeatureDataSources.cc
    ag_FeatureLayer.cc
    ag_FeatureLayerDrawer.cc
    ag_GeneralDataPropertiesWidget.cc
    ag_GeneralPreferencesWidget.cc
    ag_GeometryLegendBody.cc
    ag_IVisualisation.cc
    ag_LddDrawProps.cc
    ag_LddRasterDrawer.cc
    ag_LegendBody.cc
    ag_Legend.cc
    ag_LegendView.cc
    ag_LineMarker.cc
    ag_Map2D.cc
    ag_Map2DView.cc
    ag_Map2DWindow.cc
    ag_Map.cc
    ag_MapDrawer.cc
    ag_MapWindow.cc
    ag_MouseTarget.cc
    ag_MultiMap2D.cc
    ag_MultiMap2DView.cc
    ag_MultiMap2DWindow.cc
    ag_NominalDrawProps.cc
    ag_NominalRasterDrawer.cc
    ag_OrdinalDrawProps.cc
    ag_OrdinalRasterDrawer.cc
    ag_PlotVisualisation.cc
    ag_PlotView.cc
    ag_PreferencesDialog.cc
    ag_PropertiesDialog.cc
    ag_PropertiesWidget.cc
    ag_QApplication.cc
    ag_RangeDrawPropertiesWidget.cc
    ag_RangeDrawProps.cc
    ag_RangeFeatureLayerDrawer.cc
    ag_RangeLegendBody.cc
    ag_Raster.cc
    ag_RasterDataset.cc
    ag_RasterDataSources.cc
    ag_RasterDrawer.cc
    ag_SaveDataAsDialog.cc
    ag_SaveViewAsDialog.cc
    ag_ScalarRasterDrawer.cc
    ag_SpatialDataset.cc
    ag_Table.cc
    ag_TableDataSources.cc
    ag_TableVisualisation.cc
    ag_Templates.cc
    ag_Time.cc
    ag_TimePlot.cc
    ag_TimePlotWindow.cc
    ag_Util.cc
    ag_Vector.cc
    ag_VectorDataSources.cc
    ag_VectorDrawer.cc
    ag_Viewer.cc
    ag_VisChangeManager.cc
    ag_VisEngine.cc
    ag_VisGroup.cc
    ag_VisGroupManager.cc
    ag_VisObserver.cc
    ag_VisSubject.cc
    ag_Visualisation.cc
    ag_VisualisationDialog.cc
    ag_VisualisationWindow.cc
    ag_XMLViewItems.cc

    com_classifier.cc
    com_colour.cc
    com_const.cc
    com_exception.cc
    com_fileformatinfo.cc
    com_palette.cc
    com_rangemap.cc
    com_rawpalette.cc
    com_rcobject.cc
    com_rcsize_t.cc
    com_rgbtuple.cc
    com_util.cc

    geo_dataguide.cc
    geo_DataType.cc

    qt_Animation.cc
    qt_AnimationProgBar.cc
    qt_AppWindow.cc
    qt_AppWindowProperties.cc
    qt_ColourLib.cc
    qt_Dialog.cc
    qt_EventFilterSubject.cc
    qt_GuiApp.cc
    qt_New.cc
    qt_PropertiesDialog.cc
    qt_PropertiesWidget.cc
    # qt_SelectableWidget
    qt_Util.cc

    qtd_SelectPalette.cc

    qtw_PaletteBar.cc

    # Plots/ag_ProbabilityPlot
    # Plots/ag_ProbabilityTimeSeriesPlot
    # Plots/ag_ProbabilityValuePlot
    # Plots/ag_TimeSeriesPlot
    # Plots/ag_ValueTimeSeriesPlot

    ${AGUILA_OPENGL_SOURCES}

)

target_include_directories(pcraster_aguila_objects
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
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
if(PCRASTER_WITH_OPENGL)
    target_link_libraries(pcraster_aguila_objects
        PRIVATE
            pcraster::xsd
            Qt5::OpenGL
            Qt5::Xml
    )
else()
    target_link_libraries(pcraster_aguila_objects
        PRIVATE
            pcraster::xsd
            Qt5::Widgets
            Qt5::Xml
    )
endif()

target_compile_definitions(pcraster_aguila_objects
    PRIVATE
        PCR_DAL_SHARED_LINK
        PCR_AG_SOURCE
        PCR_AG_SHARED_LINK
)


if(PCRASTER_WITH_OPENGL)
  target_compile_definitions(pcraster_aguila_objects
    PRIVATE
      AGUILA_WITH_OPENGL
  )
endif()

add_library(pcraster_aguila SHARED
    $<TARGET_OBJECTS:pcraster_aguila_objects>
)

add_library(pcraster::aguila ALIAS pcraster_aguila)

set_target_properties(pcraster_aguila
    PROPERTIES
        SOVERSION ${${PROJECT_NAME}_MAJOR_VERSION}
        VERSION ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}
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
            Boost::headers
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
            Boost::headers
    )
endif()


install(
    TARGETS pcraster_aguila
    RUNTIME DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
    LIBRARY DESTINATION ${PCRASTER_LIB_INSTALL_DIR} COMPONENT Runtime
)

add_executable(aguila
    WIN32 # Use WinMain instead of main. Gets rid of Dos box.
    main.cc
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
        ag_AguilaProgramOptionsTest.cc
        ag_ClassifierTest.cc
        ag_ColourSelectorTest.cc
        DataObjectBaseTest.cc
        DataManagerTest.cc
        com_fileformatinfotest.cc
        com_matrixtest.cc
        com_rangemaptest.cc
        com_rgbtupletest.cc
        com_utiltest.cc
        com_userdefinedclassifiertest.cc
        com_vectortest.cc
        # SUPPORT_NAMES ag_AguilaGuiTest  # Only needed for AguilaTest.
    )
    set_property(
        SOURCE ${AGUILA_TEST_SOURCES}
        PROPERTY COMPILE_DEFINITIONS
            # PCR_AG_SOURCE
            PCR_AG_SHARED_LINK
    )
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
