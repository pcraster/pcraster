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
    ${CMAKE_CURRENT_SOURCE_DIR}/../api
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../calc
    ${CMAKE_CURRENT_SOURCE_DIR}/../geom
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../pccts
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrdiscr
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrxml
    ${CMAKE_CURRENT_SOURCE_DIR}/../tab
)

ADD_DEFINITIONS(-DUSE_IN_PCRCALC -DYY_NO_UNISTD_H -DYY_NEVER_INTERACTIVE)

SET(SOURCES
    calc_stattable.cc
    calc_repeatuntil.cc
    calc_valuebuffer.cc
    calc_compressor.cc
    calc_maskcompressor.cc
    calc_nullcompressor.cc
    calc_compressioninput.cc
    calc_decompresseddata.cc
    calc_globargs.cc
    calc_globresult.cc
    calc_globarg.cc
    calc_spatial.cc
    calc_nonspatial.cc
    calc_field.cc
    calc_gridmap.cc
    calc_zeromap.cc
    calc_iocsffieldstrategy.cc
    calc_ioesrifieldstrategy.cc
    calc_iofieldstrategy.cc
    calc_iobandfieldstrategy.cc
    calc_bandmap.cc
    calc_branchexpr.cc
    calc_branchexprimpl.cc
    calc_constant.cc
    calc_constantnumbertype.cc
    calc_csfmap.cc
    calc_doubleass.cc
    calc_esrimap.cc
    calc_esrigridio.cc
    calc_liberror.cc
    calc_generatespatialfunc.cc
    calc_quote.cc
    calc_rundirectory.cc
    calc_stackreader.cc
    calc_manualexampletester.cc
    calc_positionname.cc
    calc_lexinput.cc
    calc_lexinputsource.cc
    calc_extsym.cc
    calc_lexinputsourcetextscript.cc
    calc_lexinputsourcestring.cc
    calc_position.cc
    calc_positiontext.cc
    calc_positionnone.cc
    calc_modelbuilder.cc
    calc_wldelfthabitat.cc
    calc_reporttable.cc
    calc_bindingtable.cc
    calc_parserinput.cc
    calc_modelparser.cc
    calc_element.cc
    calc_quitforexitoption.cc
    calc_quitforprogresscallback.cc
    calc_quitpremature.cc
    calc_progresscallback.cc
    calc_clientinterface.cc
    calc_iclientinterface.cc
    calc_runsettings.cc
    calc_calc.cc
    calc_dllcalc.cc
    calc_script.cc
    calc_iscript.cc
    calc_apimap.cc
    calc_arraydefinition.cc
    calc_arraydefvector.cc
    calc_assignment.cc
    calc_bindedsymbol.cc
    calc_blockposition.cc
    calc_dynamicsection.cc
    calc_fieldargs.cc
    calc_fieldexpr.cc
    calc_fieldleaf.cc
    calc_fieldleft.cc
    calc_fieldmapinputparameter.cc
    calc_fieldnewparameter.cc
    calc_fieldnrparameter.cc
    calc_fieldparameter.cc
    calc_fieldstack.cc
    calc_fieldtype.cc
    calc_fieldvalue.cc
    calc_file.cc
    calc_filewriter.cc
    calc_foreach.cc
    calc_idlist.cc
    calc_indexcontainer.cc
    calc_indexparameter.cc
    calc_indexparameterconstant.cc
    calc_indexparametervariable.cc
    calc_indexselectedsingle.cc
    calc_indexselectedvector.cc
    calc_indexset.cc
    calc_indextable.cc
    calc_infoscript.cc
    calc_innerstatementblock.cc
    calc_lookupexpr.cc
    calc_cmdlinecalc.cc
    calc_methodoperator.cc
    calc_mixedexpr.cc
    calc_modellinkmethodexpr.cc
    calc_modellinkinit.cc
    calc_modellinkmethodstatement.cc
    calc_nonspatialimpl.cc
    calc_nonspatialtssimpl.cc
    calc_objcount.cc
    calc_operator.cc
    calc_parameter.cc
    calc_parsindex.cc
    calc_parspar.cc
    calc_posexception.cc
    calc_report.cc
    calc_spatialimpl.cc
    calc_stackcr.cc
    calc_stackinput.cc
    calc_statement.cc
    calc_statementblock.cc
    calc_stdoutstat.cc
    calc_subparameter.cc
    calc_symbol.cc
    calc_lookuptableleaf.cc
    calc_lookuptableparameter.cc
    calc_lookuptable.cc
    calc_lookuplinear.cc
    calc_timeinputexpr.cc
    calc_timeoutput.cc
    calc_tssinputparameter.cc
    calc_timetable.cc
    calc_tssinputleaf.cc
    calc_tssoutputparameter.cc
    calc_tssoutputvalue.cc
    calc_tssparameter.cc
    calc_usedefnode.cc
    calc_usermodellink.cc
    calc_usersymbol.cc
    calc_vs.cc
    calc_writeinfo.cc
    calc_timervalue.cc
    calc_symboltable.cc
    calc_defpar.cc
    calc_usepar.cc
    calc_map2csf.cc
    lexvaluefile.cc
    lexnewcalc.cc
    Parser.cc
    calcparser.cc
    calc_externalsymbols.cc
    calc_findsymbol.cc
    pcrcalc.cc
    calc_arcviewextapi.cc
    dassfunc.c
    dbinfunc.c
    dunfunc.c
    genfunc.c
    globfunc.c
    sbinfunc.c
    sunfunc.c
)

# SET(TARGET_LINK_LIBS
#     calc
#     pcrgeo
#     ${PCRASTER_DAL_LIBRARY}
#     pcrcom
#     tab
#     app
#     api
#     ${PCRASTER_RASTERFORMAT_LIBRARY}
#     geom
#     mathx
#     misc
#     pcrxml
#     pccts
#     ${QT_QTXML_LIBRARY}
#     ${QT_QTCORE_LIBRARIES}
#     ${BOOST_FILESYSTEM_LIBRARY}
#     ${BOOST_MATH_C99_LIBRARY}
#     ${BOOST_SYSTEM_LIBRARY}
# )
ADD_LIBRARY(pcraster_old_calc
    SHARED
    ${SOURCES}
)

# IF(WIN32)
#     TARGET_LINK_LIBRARIES(pcraster_old_calc
#         ${TARGET_LINK_LIBS}
#     )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(pcraster_old_calc
        pcraster_dal
        calc
        pcrxml
        pccts
        # ${Boost_MATH_C99_LIBRARY}
    )
# ENDIF(WIN32)

INSTALL(
    TARGETS pcraster_old_calc
    RUNTIME DESTINATION lib COMPONENT Runtime
    LIBRARY DESTINATION lib COMPONENT Runtime
)

if(FALSE AND PCRASTER_BUILD_TEST)
    SET(TEST_SOURCES
        calc_test.cc
        calc_tssoutputvaluetest.cc
        calc_maskcompressortest.cc
        calc_manualexampletestertest.cc
        calc_manualexamplestest.cc
        calc_nonspatialtest.cc
        calc_spatialtest.cc
        calc_lookuptabletest.cc
        calc_lookuplineartest.cc
        # calc_clientinterfacetest.cc
        calc_rundirectorytest.cc
        calc_iocsffieldstrategytest.cc
        # calc_iobandfieldstrategytest.cc
        # calc_iobandtest.cc
        calc_parsertest.cc
        calc_lexinputtest.cc
        calc_modelbuildertest.cc
        # calc_wldelfthabitattest.cc
    )

    # ADD_LIBRARY(pcraster_old_calc_static
    #     STATIC
    #     ${SOURCES}
    # )
    ADD_EXECUTABLE(test_pcraster_old_calc
        ${TEST_SOURCES}
    )

    # IF(WIN32)
    #     TARGET_LINK_LIBRARIES(test_pcraster_old_calc
    #         pcraster_old_calc_static
    #         ${TARGET_LINK_LIBS}
    #     )
    # ELSE(WIN32)
        TARGET_LINK_LIBRARIES(test_pcraster_old_calc
            pcraster_old_calc # _static
            # ${TARGET_LINK_LIBS}
        )
    # ENDIF(WIN32)

    ADD_CUSTOM_TARGET(run_test_pcraster_old_calc
        COMMAND bash ./testrun.prolog
        COMMAND test_pcraster_old_calc --detect_memory_leaks=0
        COMMAND bash ./testrun.epilog
        DEPENDS test_pcraster_old_calc
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    ADD_DEPENDENCIES(tests run_test_pcraster_old_calc)
endif()
