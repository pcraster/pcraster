# Note that a lot of generated stuff is simply put in svn
# to recreate pull stuff from OLDPCRTREE
# LATER DITCH PCRXML?
add_library(pcrxml STATIC
    pcrxml_attribute
    pcrxml_bindoublele
    pcrxml_childelementvisitor
    pcrxml_csfvs2datatype
    pcrxml_date
    pcrxml_doctype
    pcrxml_document
    pcrxml_dom
    pcrxml_domdiff
    pcrxml_element
    pcrxml_interspacedsentence
    pcrxml_newdatasubtype
    pcrxml_ostream
    pcrxml_pcdata
    pcrxml_pcdataelement
    pcrxml_pcrvs2datatype
    pcrxml_simpleattr
    pcrxml_string
    pcrxml_stringconv
    pcrxml_word

    # Generated but now fixed, pcrxsd supersedes -------------------------------
    pcrgenxml_areamapdtd
    pcrgenxml_binding
    pcrgenxml_coefficient
    pcrgenxml_compute
    pcrgenxml_computedriveraxis
    pcrgenxml_cursor
    pcrgenxml_data
    pcrgenxml_dataenvelop
    pcrgenxml_dataenvelopencoding
    pcrgenxml_dataextend
    pcrgenxml_dataobject
    pcrgenxml_datasetconfiguration
    pcrgenxml_datatypedtd
    pcrgenxml_datatypeenum
    pcrgenxml_dateformat
    pcrgenxml_dimension
    pcrgenxml_dimensionbaseenum
    pcrgenxml_directorystackinfo
    pcrgenxml_exchangedirection
    pcrgenxml_exchangeitem
    pcrgenxml_exchangemodel
    pcrgenxml_expirationdate
    pcrgenxml_fileinput
    pcrgenxml_filesetting
    pcrgenxml_input
    pcrgenxml_inputfile
    pcrgenxml_inputlodings
    pcrgenxml_inputpoints
    pcrgenxml_inputtype
    pcrgenxml_integertimer
    pcrgenxml_interpolate
    pcrgenxml_iostrategy
    pcrgenxml_iostrategytype
    pcrgenxml_iotype
    pcrgenxml_lodinginputtype
    pcrgenxml_lodingname
    pcrgenxml_lodings
    pcrgenxml_lowerlimit
    pcrgenxml_map
    pcrgenxml_migrationdirection
    pcrgenxml_modelrunchild
    pcrgenxml_modelrunsettings
    pcrgenxml_modelrunstatus
    pcrgenxml_nonspatial
    pcrgenxml_numericinput
    pcrgenxml_numericsetting
    pcrgenxml_outputtype
    pcrgenxml_paneltype
    pcrgenxml_parameteritem
    pcrgenxml_parameterpanel
    pcrgenxml_rastermask
    pcrgenxml_rasterspace
    pcrgenxml_resultlastrun
    pcrgenxml_riveraxisfile
    pcrgenxml_rundirectory
    pcrgenxml_runtimeplatform
    pcrgenxml_script
    pcrgenxml_scriptdata
    pcrgenxml_scriptlink
    pcrgenxml_scripttype
    pcrgenxml_showdata
    pcrgenxml_spatial
    pcrgenxml_stack
    pcrgenxml_table
    pcrgenxml_testemptyelementnoattr
    pcrgenxml_timer
    pcrgenxml_timeseries
    pcrgenxml_timesteprange
    pcrgenxml_upperlimit
    pcrgenxml_userderive
    pcrgenxml_userinterfacedescription
    pcrgenxml_variable
    pcrgenxml_visualisationconfiguration
    pcrgenxml_visualisationgroup
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
        Qt5::Xml
)

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_test(
        NAME setup_pcrxml
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_pcrxml
        PROPERTIES FIXTURES_SETUP FixturePcrXml
    )

    add_unit_tests(
        SCOPE pcrxml
        NAMES
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
