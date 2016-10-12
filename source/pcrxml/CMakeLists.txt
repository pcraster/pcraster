# Note that a lot of generated stuff is simply put in svn
# to recreate pull stuff from OLDPCRTREE
# LATER DITCH PCRXML?

# ADD_DEFINITIONS(-DQT3_SUPPORT)

CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
)
CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.epilog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.epilog
    @ONLY
)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
)
# INCLUDE_DIRECTORIES(
#    ../mathx
# )


SET(SOURCES
    pcrxml_attribute.cc
    pcrxml_bindoublele.cc
    pcrxml_childelementvisitor.cc
    pcrxml_csfvs2datatype.cc
    pcrxml_date.cc
    pcrxml_doctype.cc
    pcrxml_document.cc
    pcrxml_dom.cc
    pcrxml_domdiff.cc
    pcrxml_element.cc
    pcrxml_interspacedsentence.cc
    pcrxml_newdatasubtype.cc
    pcrxml_ostream.cc
    pcrxml_pcdata.cc
    pcrxml_pcdataelement.cc
    pcrxml_pcrvs2datatype.cc
    pcrxml_simpleattr.cc
    pcrxml_string.cc
    pcrxml_stringconv.cc
    pcrxml_word.cc
)
# Generated but now fixed, pcrxsd supersedes
SET(GENSOURCES
    pcrgenxml_areamapdtd.cc
    pcrgenxml_binding.cc
    pcrgenxml_coefficient.cc
    pcrgenxml_compute.cc
    pcrgenxml_computedriveraxis.cc
    pcrgenxml_cursor.cc
    pcrgenxml_data.cc
    pcrgenxml_dataenvelop.cc
    pcrgenxml_dataenvelopencoding.cc
    pcrgenxml_dataextend.cc
    pcrgenxml_dataobject.cc
    pcrgenxml_datasetconfiguration.cc
    pcrgenxml_datatypedtd.cc
    pcrgenxml_datatypeenum.cc
    pcrgenxml_dateformat.cc
    pcrgenxml_dimension.cc
    pcrgenxml_dimensionbaseenum.cc
    pcrgenxml_directorystackinfo.cc
    pcrgenxml_exchangedirection.cc
    pcrgenxml_exchangeitem.cc
    pcrgenxml_exchangemodel.cc
    pcrgenxml_expirationdate.cc
    pcrgenxml_fileinput.cc
    pcrgenxml_filesetting.cc
    pcrgenxml_input.cc
    pcrgenxml_inputfile.cc
    pcrgenxml_inputlodings.cc
    pcrgenxml_inputpoints.cc
    pcrgenxml_inputtype.cc
    pcrgenxml_integertimer.cc
    pcrgenxml_interpolate.cc
    pcrgenxml_iostrategy.cc
    pcrgenxml_iostrategytype.cc
    pcrgenxml_iotype.cc
    pcrgenxml_lodinginputtype.cc
    pcrgenxml_lodingname.cc
    pcrgenxml_lodings.cc
    pcrgenxml_lowerlimit.cc
    pcrgenxml_map.cc
    pcrgenxml_migrationdirection.cc
    pcrgenxml_modelrunchild.cc
    pcrgenxml_modelrunsettings.cc
    pcrgenxml_modelrunstatus.cc
    pcrgenxml_nonspatial.cc
    pcrgenxml_numericinput.cc
    pcrgenxml_numericsetting.cc
    pcrgenxml_outputtype.cc
    pcrgenxml_paneltype.cc
    pcrgenxml_parameteritem.cc
    pcrgenxml_parameterpanel.cc
    pcrgenxml_rastermask.cc
    pcrgenxml_rasterspace.cc
    pcrgenxml_resultlastrun.cc
    pcrgenxml_riveraxisfile.cc
    pcrgenxml_rundirectory.cc
    pcrgenxml_runtimeplatform.cc
    pcrgenxml_script.cc
    pcrgenxml_scriptdata.cc
    pcrgenxml_scriptlink.cc
    pcrgenxml_scripttype.cc
    pcrgenxml_showdata.cc
    pcrgenxml_spatial.cc
    pcrgenxml_stack.cc
    pcrgenxml_table.cc
    pcrgenxml_testemptyelementnoattr.cc
    pcrgenxml_timer.cc
    pcrgenxml_timeseries.cc
    pcrgenxml_timesteprange.cc
    pcrgenxml_upperlimit.cc
    pcrgenxml_userderive.cc
    pcrgenxml_userinterfacedescription.cc
    pcrgenxml_variable.cc
    pcrgenxml_visualisationconfiguration.cc
    pcrgenxml_visualisationgroup.cc
)
ADD_LIBRARY(pcrxml
    ${GENSOURCES}
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(pcrxml
    pcrcom
    Qt5::Xml
)


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        pcrxml_attributetest
        pcrxml_bindoubleletest
        pcrxml_childelementvisitortest
        pcrxml_documenttest
        pcrxml_domalgorithmtest
        pcrxml_domtest
        pcrxml_elementtest
        pcrxml_enumnmtokentest
        pcrxml_interspacedsentencetest
        pcrxml_pcdataelementtest
        pcrxml_simpleattrtest
        pcrxml_wordtest
    )
    add_unit_tests(
        SCOPE pcrxml
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            pcrxml
    )
endif()
