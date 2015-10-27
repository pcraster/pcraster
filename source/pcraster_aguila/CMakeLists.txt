set(AGUILA_MAJOR_VERSION 1)
set(AGUILA_MINOR_VERSION 3)
set(AGUILA_PATCH_VERSION 3)
set(AGUILA_VERSION
    "${AGUILA_MAJOR_VERSION}.${AGUILA_MINOR_VERSION}.${AGUILA_PATCH_VERSION}")

set(CMAKE_AUTOMOC ON)

CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/ag_Configure.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/../include/ag_Configure.h
)
CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_xsd
    ${CMAKE_CURRENT_BINARY_DIR}/../pcraster_xsd
    ${CMAKE_CURRENT_BINARY_DIR}/../pcraster_aguila
    ${CMAKE_CURRENT_SOURCE_DIR}/../math_utils
    ${CMAKE_CURRENT_SOURCE_DIR}/../string_utils
    ${CMAKE_CURRENT_SOURCE_DIR}/../utils
)

SET(LIB_SOURCES
    ag_Aguila.cc
    ag_AguilaProgramOptions.cc
    ag_AnimationControl.cc
    ag_BooleanDrawProps.cc
    ag_BooleanRasterDrawer.cc
    ag_BufferedVisualisation.cc
    ag_BufferedWidget.cc
    ag_Camera.cc
    ag_ClassDrawPropertiesWidget.cc
    ag_ClassDrawProps.cc
    ag_ClassLegendBody.cc
    ag_ColourSelector.cc
    ag_Configurable.cc
    ag_Cube.cc
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
    ag_Feedback.cc
    # ag_FileIconProvider.cc
    ag_GeneralDataPropertiesWidget.cc
    ag_GeneralPreferencesWidget.cc
    ag_GeometryLegendBody.cc
    ag_GLVisualisation.cc
    ag_IVisualisation.cc
    ag_LddDrawProps.cc
    ag_LddRasterDrawer.cc
    ag_LegendBody.cc
    ag_Legend.cc
    ag_LegendView.cc
    ag_Map2D.cc
    ag_Map2DView.cc
    ag_Map2DWindow.cc
    ag_Map3D.cc
    ag_Map3DObject.cc
    ag_Map3DView.cc
    ag_Map3DWindow.cc
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
    ag_PostScript.cc
    ag_PreferencesDialog.cc
    ag_PropertiesDialog.cc
    ag_PropertiesWidget.cc
    ag_QApplication.cc
    ag_QuadDrawer.cc
    ag_Quaternion.cc
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
    ag_Scene.cc
    ag_SceneObject.cc
    ag_SceneView.cc
    ag_ScriptEdit.cc
    ag_ScriptEditWindow.cc
    ag_ScriptSyntaxHighlighter.cc
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
    # qt_SelectableWidget.cc
    qt_Util.cc

    qtd_SelectPalette.cc

    qtw_PaletteBar.cc

    # Plots/ag_ProbabilityPlot.cc
    # Plots/ag_ProbabilityTimeSeriesPlot.cc
    # Plots/ag_ProbabilityValuePlot.cc
    # Plots/ag_TimeSeriesPlot.cc
    # Plots/ag_ValueTimeSeriesPlot.cc
)
QT4_WRAP_UI(
    UI_SOURCES
    # CursorValueDialog.ui
    GeneralPreferencesWidgetBase.ui
    PreferencesDialogBase.ui
    SaveDataAsDialogBase.ui
    SaveViewAsDialogBase.ui
)

SET(LIBRARY_SOURCES
    ${LIB_SOURCES}
    ${UI_SOURCES}
)

### Since the sources are used by the static and shared lib, automoc seems to
### process each file twice. This results in conflicts. Commenting out the
### build of the static lib for now.
### IF(WIN32)
###     ADD_LIBRARY(pcraster_aguila_static
###         STATIC
###         ${LIBRARY_SOURCES}
###     )
###     SET_TARGET_PROPERTIES(pcraster_aguila_static
###         PROPERTIES
###             COMPILE_FLAGS "-DPCR_AG_SOURCE"
###     )
###     # Only build the static lib once the shared lib is built. Building them
###     # together seems to result in conflicts. I think automoc may run for one
###     # lib target while the other lib target is being build, locking the files
###     # that are being generated.
###     ADD_DEPENDENCIES(pcraster_aguila_static pcraster_aguila)
### ENDIF()

ADD_LIBRARY(pcraster_aguila
    SHARED
    ${LIBRARY_SOURCES}
)
SET_TARGET_PROPERTIES(pcraster_aguila
    PROPERTIES
        COMPILE_FLAGS "-DPCR_AG_SOURCE -DPCR_AG_SHARED_LINK"
)
TARGET_LINK_LIBRARIES(pcraster_aguila
    pcraster_dal
    pcraster_xsd
    Qt4::QtGui
    Qt4::QtOpenGL
    ${QWT_LIBRARY}
    ${OPENGL_LIBRARIES}
)
# ADD_CUSTOM_COMMAND(
#     TARGET pcraster_aguila
#     POST_BUILD
#         COMMAND ${CMAKE_COMMAND} -E
#              make_directory ${CMAKE_BINARY_DIR}/share/gdal
#         COMMAND ${CMAKE_COMMAND} -E
#              copy_directory ${GDAL_DATA} ${CMAKE_BINARY_DIR}/share/gdal
# )

INSTALL(
    TARGETS pcraster_aguila
    RUNTIME DESTINATION lib COMPONENT Runtime
    LIBRARY DESTINATION lib COMPONENT Runtime
)

SET(APP_SOURCES
    main.cc
)

ADD_EXECUTABLE(aguila
    WIN32 # Use WinMain instead of main. Gets rid of Dos box.
    ${APP_SOURCES}
)
SET_TARGET_PROPERTIES(aguila
    PROPERTIES
        COMPILE_FLAGS "-DPCR_AG_SHARED_LINK"
)
TARGET_LINK_LIBRARIES(aguila
    pcraster_aguila
    ${QT_QTMAIN_LIBRARY}  # Contains WinMain.
)
INSTALL(
    TARGETS aguila
    RUNTIME DESTINATION bin COMPONENT Runtime
)

if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        # ag_AguilaTest  # Requires an X-server.
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
    )
    add_unit_tests(
        SCOPE pcraster_aguila
        NAMES ${TEST_NAMES}
        # SUPPORT_NAMES ag_AguilaGuiTest  # Only needed for AguilaTest.
        LINK_LIBRARIES
            pcraster_aguila
            ${QT_QTTEST_LIBRARY}
    )
endif()
