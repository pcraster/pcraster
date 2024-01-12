# Note that a lot of generated stuff is simply put in svn
# to recreate pull stuff from OLDPCRTREE
# LATER DITCH PCRXML?
add_library(pcrxml STATIC
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

    # Generated but now fixed, pcrxsd supersedes -------------------------------
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
    # --------------------------------------------------------------------------
)

add_library(pcraster::pcrxml ALIAS pcrxml)

target_include_directories(pcrxml
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcrxml
    PUBLIC
        pcraster::pcrcom
        Qt::Xml
)

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_test(
        NAME setup_pcrxml
        COMMAND ${Python_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_pcrxml
        PROPERTIES FIXTURES_SETUP FixturePcrXml
    )

    add_unit_tests(
        SCOPE pcrxml
        NAMES
            pcrxml_attributetest.cc
            pcrxml_bindoubleletest.cc
            pcrxml_childelementvisitortest.cc
            pcrxml_documenttest.cc
            pcrxml_domalgorithmtest.cc
            pcrxml_domtest.cc
            pcrxml_elementtest.cc
            pcrxml_enumnmtokentest.cc
            pcrxml_interspacedsentencetest.cc
            pcrxml_pcdataelementtest.cc
            pcrxml_simpleattrtest.cc
            pcrxml_wordtest.cc
        LINK_LIBRARIES
            pcraster::pcrxml
    )

    set_tests_properties(setup_pcrxml
        PROPERTIES
            FIXTURES_SETUP
                FixturePcrXml
    )

    set_tests_properties(pcrxml_pcrxml_attributetest
        PROPERTIES
            FIXTURES_REQUIRED
                FixturePcrXml
    )

endif()
