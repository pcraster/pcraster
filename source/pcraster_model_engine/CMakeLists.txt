configure_file(
    operationExpanded.xml.in
    operationExpanded.xml
    @ONLY
    NEWLINE_STYLE UNIX
)

add_compile_definitions(USE_IN_PCRCALC)

if(PCRASTER_INSTALL_FILES_DEVELOPER)
  add_subdirectory(testdata/apiExamples)
  add_subdirectory(testdata/apiExamplesData)
  add_subdirectory(csharp)
endif()

# actually newcalc.l
add_file_dependency(lexnewcalc.cc
    ${CMAKE_CURRENT_BINARY_DIR}/major_op.h)
add_file_dependency(calc_findsymbol.h
    ${CMAKE_CURRENT_BINARY_DIR}/major_op.h)
add_file_dependency(calc_operation.h
    ${CMAKE_CURRENT_BINARY_DIR}/major_op.h)
add_file_dependency(calc_operations.h
    ${CMAKE_CURRENT_BINARY_DIR}/major_op.h)
add_file_dependency(calc_operator.h
    ${CMAKE_CURRENT_BINARY_DIR}/major_op.h)
add_file_dependency(lextoken.h
    ${CMAKE_CURRENT_BINARY_DIR}/major_op.h)
add_file_dependency(calc_fopointarraytest.cc
    ${CMAKE_CURRENT_BINARY_DIR}/calc_fopointarrayimpl.inc)
add_file_dependency(calc_fopointarraytest.cc
    ${CMAKE_CURRENT_BINARY_DIR}/calc_fopointimpl.h)
add_file_dependency(calc_operations.cc
    ${CMAKE_CURRENT_BINARY_DIR}/calc_fopointimpl.h)
add_file_dependency(calc_pointcodedllheader.h
    ${CMAKE_CURRENT_BINARY_DIR}/calc_fopointimpl.h)
add_file_dependency(calc_operations.cc
    ${CMAKE_CURRENT_BINARY_DIR}/calc_fopointarrayimpl.inc)
add_file_dependency(calc_operations.cc
    ${CMAKE_CURRENT_BINARY_DIR}/calc_functiontable.h)
add_file_dependency(calc_operations.cc
    ${CMAKE_CURRENT_BINARY_DIR}/operationsInit.inc)
add_file_dependency(calc_operations.cc
    ${CMAKE_CURRENT_BINARY_DIR}/calc_builtinops.inc)

# operationExpanded.xml was previously generated each time at a build.
# Due to removal of xsltproc dependency this file needs to be
# regenerated when the source files change
# Hash the copied file in build directory rather than the source due to different UNIX/WIN line endings
file(SHA1 ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml operation_expanded)

if( NOT ${operation_expanded} STREQUAL "c1d7687a22a7e88a3637515554dd54e4f3779c2d" )
    message(FATAL_ERROR "The operationExpanded.xml file is older than one of its dependencies. Update operationExpanded.xml.in first; or contact the PCRaster Research and Development team in case you don't know how to proceed.")
    # The previous command was part of the custom command below
    # COMMAND ${LIBXSLT_XSLTPROC_EXECUTABLE} --nonet --output ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml ${CMAKE_CURRENT_SOURCE_DIR}/../xslt/expand.xsl ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml
    # in addition, also do the xmllint check on operation.xml
endif()

# OLS: keep this command fttb; it's a dependency for the command below;
#      also for documentation purposes in case we need to regenerate operationExpanded.xml.in
# workaround: operationExpanded.xml in the build directory somehow gets removed, copy it again...
ADD_CUSTOM_COMMAND(
    OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml ${CMAKE_CURRENT_SOURCE_DIR}/operation.dtd
    COMMENT "Expanding XML to OBJECT directory"
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/operationExpanded.xml.in ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml
    # COMMAND ${LIBXML2_XMLLINT_EXECUTABLE} --noout --nonet --nocatalogs --dtdvalid ${CMAKE_CURRENT_SOURCE_DIR}/operation.dtd ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml
    # Commented due to removal of xsltproc dependency
    # COMMAND ${LIBXSLT_XSLTPROC_EXECUTABLE} --nonet --output ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml ${CMAKE_CURRENT_SOURCE_DIR}/../xslt/expand.xsl ${CMAKE_CURRENT_SOURCE_DIR}/operation.xml
)

SET(OPCODE ${CMAKE_CURRENT_BINARY_DIR}/major_op.h)

ADD_CUSTOM_COMMAND(
    OUTPUT  ${OPCODE}
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml
    COMMENT "Generating code"
    COMMAND ${Python_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/createOperatorCppCode.py
        ${CMAKE_CURRENT_BINARY_DIR}/operationExpanded.xml
)

ADD_CUSTOM_COMMAND(
    OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/calc_fopointarrayimpl.inc
    COMMAND echo  up to date
    DEPENDS ${OPCODE}
)
ADD_CUSTOM_COMMAND(
    OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/calc_fopointimpl.h
    COMMAND echo  up to date
    DEPENDS ${OPCODE}
)
ADD_CUSTOM_COMMAND(
    OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/calc_functiontable.h
    COMMAND echo  up to date
    DEPENDS ${OPCODE}
)
ADD_CUSTOM_COMMAND(
    OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/operationsInit.inc
    COMMAND echo  up to date
    DEPENDS ${OPCODE}
)
ADD_CUSTOM_COMMAND(
    OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/calc_builtinops.inc
    COMMAND echo  up to date
    DEPENDS ${OPCODE}
)

add_library(pcraster_model_engine_objects OBJECT
    calc_MemoryExchangeItem.cc
    calc_MemoryExchangeItemString.cc
    calc_MemoryExchangeItemField.cc
    calc_diskwrittenfield.cc
    calc_DynamicMemoryInput.cc
    calc_xmlcontext.cc
    calc_xmldatatype.cc
    calc_linkinexpr.cc
    calc_linkinlibrary.cc
    calc_mapstacktype.cc
    calc_vspatial.cc
    calc_iostrategy.cc
    calc_areamap.cc
    calc_timer.cc
    calc_xmlreflection.cc
    calc_dimensionparser.cc
    calc_dimension.cc
    calc_astdefinition.cc
    calc_objectlinkruntime.cc
    calc_calclib.cc
    calc_clientinterface.cc
    calc_xmlscriptclientinterface.cc
    calc_textscriptclientinterface.cc
    pcrcalc.cc
    calc_operator.cc
    calc_objectlinkmeta.cc
    calc_objectlink.cc
    calc_scopedlddgraph.cc
    calc_rttypecheck.cc
    calc_dynamicwave.cc
    calc_relationrecord.cc
    calc_cellop.cc
    calc_icelliterator.cc
    calc_code.cc
    calc_opimplredirect.cc
    calc_icachedobject.cc
    calc_downstreamvisitor.cc
    calc_TimeSliceVisitor.cc
    calc_accuimpl.cc
    calc_vfield.cc
    calc_parpcb.cc
    calc_rastergraph.cc
    calc_lddgraph.cc
    calc_averagemap.cc
    calc_orderoperations.cc
    calc_areaoperations.cc
    calc_argorder.cc
    calc_argorderidinfo.cc
    calc_pointcodeblockdll.cc
    calc_datatype.cc
    calc_datatypeclash.cc
    calc_datatypechanger.cc
    calc_buildtypesvisitor.cc
    calc_operations.cc
    calc_calc.cc
    calc_parset.cc
    calc_insertpointcodeblocks.cc
    calc_astnodecontainer.cc
    calc_pointcodeblock.cc
    calc_pointcodeblockreplacer.cc
    calc_pointcodebodygenerator.cc
    calc_pointimpl.cc
    calc_nonassexpr.cc
    calc_progresscallback.cc
    calc_cmdlinecalc.cc
    calc_usedefanalyzer.cc
    calc_stackinfo.cc
    calc_stattable.cc
    calc_tssoutputvalue.cc
    calc_gridstat.cc
    calc_fieldwriter.cc
    calc_rundirectory.cc
    calc_options.cc
    calc_runtimeenvsettings.cc
    calc_iofieldstrategy.cc
    calc_basicblock.cc
    calc_blockentrance.cc
    calc_jumpnode.cc
    calc_reportvisitor.cc
    calc_field.cc
    calc_cr.cc
    calc_stackedvalue.cc
    calc_stringparser.cc
    calc_rewriteparsedast.cc
    calc_gridmap.cc
    calc_lookup.cc
    calc_timeinputtssop.cc
    calc_timeinputstackop.cc
    calc_stackinput.cc
    calc_timetable.cc
    calc_lookuptable.cc
    calc_datatable.cc
    calc_datavalue.cc
    calc_datastorageid.cc
    calc_globallibdefs.cc
    calc_spatialpacking.cc
    calc_generatefield.cc
    calc_execarguments.cc
    calc_opimpl.cc
    calc_global.cc
    calc_globargs.cc
    calc_globarg.cc
    calc_globresult.cc
    calc_apimap.cc
    calc_domainerror.cc
    calc_globfunc.cc
    calc_maskpacking.cc
    calc_asispacking.cc
    calc_unpackedsrc.cc
    calc_unpackedcreation.cc
    calc_runtimestack.cc
    calc_nonspatial.cc
    calc_spatial.cc
    calc_executor.cc
    calc_runtimeenv.cc
    calc_runtimeengine.cc
    calc_cfgvisitor.cc
    calc_cfgnode.cc
    calc_cfgcreator.cc
    calc_bindingtable.cc
    calc_reporttable.cc
    calc_astscript.cc
    calc_runsettings.cc
    calc_aststat.cc
    calcparser.cpp
    Parser.cpp
    calc_parserinput.cc
    lexnewcalc.cc
    calc_lexinput.cc
    calc_lexinputsource.cc
    calc_lexinputsourcestring.cc
    calc_lexinputsourcetextscript.cc
    calc_id.cc
    calc_report.cc
    calc_astvisitor.cc
    calc_position.cc
    calc_positiontext.cc
    calc_positionname.cc
    calc_exception.cc
    calc_posexception.cc
    calc_symexception.cc
    calc_astsymbolinfo.cc
    calc_iotype.cc
    calc_astsymboltable.cc
    calc_astid.cc
    calc_astnode.cc
    calc_astpar.cc
    calc_astass.cc
    calc_dynamicsection.cc
    calc_repeatuntil.cc
    calc_astnumber.cc
    calc_baseexpr.cc
    calc_astexpr.cc
    calc_findsymbol.cc
    calc_quote.cc
    calc_vs.cc
    calc_file.cc
    calc_map2csf.cc

    # OLS: this one was not included and is not used anywhere?
    # calc_ldddownstream
)

set_source_files_properties(calcparser.cpp
    PROPERTIES
        COMPILE_OPTIONS
            "$<$<COMPILE_LANG_AND_ID:CXX,GNU,AppleClang,Clang>:-Wno-all;-Wno-error>"
)

target_include_directories(pcraster_model_engine_objects
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<TARGET_PROPERTY:pcraster::calc,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::pcrdiscr,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::xsd,INTERFACE_INCLUDE_DIRECTORIES>
)

# We do not care much about warnings when using ...
target_include_directories(pcraster_model_engine_objects SYSTEM
    PUBLIC
        $<TARGET_PROPERTY:pcraster::pccts,INTERFACE_INCLUDE_DIRECTORIES>
)


# add_dependencies(pcraster_model_engine_objects
#     # Some of the sources include PCRasterXSD.h, which is generated by
#     # pcraster::xsd. Since we don't link the objects library against
#     # pcraster::xsd, but only use the include directories, CMake doesn't
#     # make pcraster_model_engine_objects depend on pcraster::xsd. We need
#     # to do that ourselves. If not, PCRasterXSD.h may not be there during
#     # the build of the objects in the objects library.
#     pcraster::xsd
# )

# Is the above obsolete when using target_link_libraries?
target_link_libraries(pcraster_model_engine_objects
    PRIVATE
        pcraster::xsd
        Boost::headers
        Qt::Core
        GDAL::GDAL
)

target_include_directories(pcraster_model_engine_objects
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

# Add flags required for Qt 6
if ((MSVC) AND (MSVC_VERSION GREATER_EQUAL 1914))
    target_compile_options(pcraster_model_engine_objects PRIVATE "/Zc:__cplusplus" "/permissive-")
endif()

target_compile_definitions(pcraster_model_engine_objects
    PRIVATE
        USE_IN_PCRCALC
        YY_NO_UNISTD_H
        YY_NEVER_INTERACTIVE
        PCR_DAL_SHARED_LINK
        pcraster_model_engine_EXPORTS

        # DO NOT DO _HAS_ITERATOR_DEBUGGING=0
        # see bugzilla 178
)

add_library(pcraster_model_engine SHARED
    $<TARGET_OBJECTS:pcraster_model_engine_objects>
)

add_library(pcraster::model_engine ALIAS pcraster_model_engine)

generate_export_header(pcraster_model_engine
    BASE_NAME pcraster_model_engine
    EXPORT_MACRO_NAME PCR_ME_EXPORT
)

set_target_properties(pcraster_model_engine
    PROPERTIES
        SOVERSION ${${PROJECT_NAME}_MAJOR_VERSION}
        VERSION ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}
)

target_include_directories(pcraster_model_engine
    PUBLIC
        $<TARGET_PROPERTY:pcraster_model_engine_objects,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(pcraster_model_engine
    PUBLIC
        pcraster::calc
        pcraster::pcrdiscr
        pcraster::pccts
        pcraster::tab
        pcraster::xsd
)

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_library(pcraster_model_engine_unit_test_utils STATIC
          # utils of unittests
          calc_astpath.cc
          calc_astcfgtester.cc
          calc_asttestfactory.cc
          calc_comparefilewithvalidated.cc
          calc_messagestestdb.cc
          calc_modelbuilder.cc
          calc_modelerrortester.cc
          calc_p5stack.cc
          calc_manualexampletester.cc
    )

    target_include_directories(pcraster_model_engine_unit_test_utils
        PRIVATE
            $<TARGET_PROPERTY:pcraster_model_engine_objects,INTERFACE_INCLUDE_DIRECTORIES>
            $<TARGET_PROPERTY:pcraster::pcrxml,INTERFACE_INCLUDE_DIRECTORIES>
    )

    target_link_libraries(pcraster_model_engine_unit_test_utils
        PRIVATE
            Qt::Xml
            pcraster::xsd
    )

    set(model_engine_test_sources
        calc_areamaptest.cc
        calc_areaoperationstest.cc
        calc_argordertest.cc
        calc_astpartest.cc
        calc_astpathtest.cc
        calc_astsymboltabletest.cc
        calc_averagemaptest.cc
        calc_bindingtabletest.cc
        calc_buildtypesvisitortest.cc
        calc_cfgcreatortest.cc
        calc_clientinterfacetest.cc
        calc_cmdlinecalctest.cc
        calc_datatypetest.cc
        calc_dimensionparsertest.cc
        calc_dynamicwavetest.cc
        calc_executortest.cc
        calc_fieldtest.cc
        calc_findsymboltest.cc
        calc_fopointarraytest.cc
        calc_iocsffieldstrategytest.cc
        # calc_ldddownstreamtest         # OLS 05/2016 this one was not in this list before
        calc_lddgraphtest.cc
        calc_lexinputtest.cc
        calc_lookuptabletest.cc
        calc_manualexampletestertest.cc
        calc_manualexamplestest.cc
        calc_maskpackingtest.cc
        # calc_modelbuildertest          # OLS 05/2016 this one is empty
        calc_nonspatialtest.cc
        calc_objectlinkmetatest.cc
        calc_objectlinktest.cc
        calc_operatortest.cc
        calc_opimpltest.cc
        calc_parsertest.cc
        calc_pointcodeblockdlltest.cc
        calc_pointcodeblockreplacertest.cc
        calc_reporttest.cc
        calc_reportvisitortest.cc
        calc_rundirectorytest.cc
        calc_runtimeenginetest.cc
        calc_runtimeenvtest.cc
        calc_spatialtest.cc
        calc_tssoutputvaluetest.cc
        calc_usedefanalyzertest.cc
        calc_vfieldtest.cc
        calc_xmlreflectiontest.cc
    )
    foreach(source in ${model_engine_test_sources})
        set_source_files_properties(${source} PROPERTIES
            COMPILE_FLAGS
                "-Dpcraster_model_engine_EXPORTS"
        )
    endforeach()

    add_unit_tests(
        SCOPE model_engine
        NAMES
            #calc_test
            # unit tests
            ${model_engine_test_sources}
        INCLUDE_DIRS
            $<TARGET_PROPERTY:pcraster_model_engine_objects,INTERFACE_INCLUDE_DIRECTORIES>
            # cpptest.cc
            ${PROJECT_BINARY_DIR}/documentation/pcraster_manual/examples
        OBJECT_LIBRARIES
            $<TARGET_OBJECTS:pcraster_model_engine_objects>
        LINK_LIBRARIES
            pcraster_model_engine_unit_test_utils
            $<TARGET_PROPERTY:pcraster_model_engine,LINK_LIBRARIES>
            pcraster::pcrxml
            pcraster::xsd
    )

    # OLS start: I think the following 5 items are not necessary,
    #     cpptest.cc seems to be built by examples.sentinel already

    # add_file_dependency(calc_manualexamplestest.cc
    #     ${CMAKE_CURRENT_BINARY_DIR}/cpptest.cc)

    ### SET(_MANUAL_DIR ${PROJECT_SOURCE_DIR}/Documentation/PCRasterManual)
    ### SET(_EXAMPLES_DIR ${_MANUAL_DIR}/examples)
    ### SET(_MANUAL_BUILD_DIR ${_MANUAL_DIR}/sphinx/_build)

    ### ADD_CUSTOM_COMMAND(
    ###   OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/cpptest.cc
    ###   DEPENDS examplesCppTests
    ###   ${PROJECT_BINARY_DIR}/Documentation/PCRasterManual/examples/cpptest.cc
    ###   # DEPENDS
    ###   #   ${_EXAMPLES_DIR}/ScriptSamples.py
    ###   #   ${_EXAMPLES_DIR}/PCRasterSample.py
    ###   #   ${_EXAMPLES_DIR}/Makefile
    ###   # COMMAND make -C ${_EXAMPLES_DIR} CPlusPlusTests OUTPUT=${CMAKE_CURRENT_BINARY_DIR}/cpptest.cc
    ### )

    # OLS end

    add_dependencies(pcraster_model_engine_unit_test_utils
        pcraster_model_engine
        generate_examples.sentinel
        # OLS: below ones from the old setup; here just added
        # mapattr asc2map
        # example1 example2 exampleCpp
    )

    add_test(
        NAME setup_model_engine
        COMMAND ${Python_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_model_engine
        PROPERTIES
            FIXTURES_SETUP
                FixturePCRasterModelEngine
    )

    set_tests_properties(
            model_engine_calc_areamaptest
            model_engine_calc_executortest
        PROPERTIES
            FIXTURES_REQUIRED
                FixturePCRasterModelEngine
    )

    #     TARGET_LINK_LIBRARIES(test_pcraster_model_engine
    #         pcraster_model_engine  # _static
    #         # ${TARGET_LINK_LIBS}
    #     )
    #
    #     ADD_CUSTOM_TARGET(run_test_pcraster_model_engine
    #         COMMAND sed -i.tmp "s/CMAKE_CFG_INTDIR/${CMAKE_CFG_INTDIR}/g"
    #             ./testrun.prolog
    #         COMMAND bash ./testrun.prolog
    #         COMMAND test_pcraster_model_engine --detect_memory_leaks=0
    #         COMMAND bash ./testrun.epilog
    #         DEPENDS test_pcraster_model_engine
    #         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    #     )
    #     ADD_DEPENDENCIES(tests run_test_pcraster_model_engine)

endif()


install(
    TARGETS pcraster_model_engine
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
    LIBRARY
        DESTINATION ${PCRASTER_LIB_INSTALL_DIR} COMPONENT Runtime
)

if(PCRASTER_INSTALL_FILES_DEVELOPER)
    install(
        FILES
            PCRaster.xsd
            ${CMAKE_CURRENT_SOURCE_DIR}/../xmlschema/commonTypes.xsd
        DESTINATION
            doc/developer/xsd
    )

    install(
        FILES
            pcrcalc.h
        DESTINATION
            doc/developer/c/include
    )
endif()

### ADD_CUSTOM_TARGET(
###   isLinkOutAPIUserManualPdfValid ALL
###   COMMAND make -C ${PROJECT_SOURCE_DIR}/documentation/user/Developer/LinkOut LinkOutAPIUserManual.pdf
###   DEPENDS ${PROJECT_SOURCE_DIR}/documentation/user/Developer/LinkOut/LinkOutAPIUserManual.odt
### )
###
### INSTALL(FILES
###     ../../../../documentation/user/Developer/LinkOut/LinkOutAPIUserManual.pdf
###   DESTINATION
###     doc/developer/linkout
### )


if(PCRASTER_BUILD_DOCUMENTATION)
    # temporary switch comments to generate LinkIn
    # LinkIn is NOT YET part of the distro!            OLS: 03/2016 I think it is distributed?!
    set(_DOX_LINKOUT_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/pcrcalc.h
        ${CMAKE_CURRENT_SOURCE_DIR}/apiDocsMain
        ${CMAKE_CURRENT_SOURCE_DIR}/../include/pcrlinkin.h
        ${CMAKE_CURRENT_SOURCE_DIR}/linkInDocsMain
    )

    # TODO make this completely working
    # SET(_DOX_LINKIN_SOURCES
    #   ${CMAKE_CURRENT_SOURCE_DIR}/pcrcalc.h
    #   ${PROJECT_SOURCE_DIR}/sources/include/pcrlinkin.h
    #   ${CMAKE_CURRENT_SOURCE_DIR}/linkInDocsMain
    # )

    set(_DOC_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/index.html)

    add_custom_command(
        OUTPUT ${_DOC_OUTPUT}
        COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/environment/script/createdoxyfile.py ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile PROJECT_NAME="\\"PCRaster LinkOut API\\"" INPUT="${_DOX_LINKOUT_SOURCES}" OUTPUT_DIRECTORY="${CMAKE_CURRENT_BINARY_DIR}"
        COMMAND doxygen ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS ${_DOX_LINKOUT_SOURCES}
    )

    ### SET(_DEPLOYMENT_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/deployment.txt)
    ### GET_TARGET_PROPERTY(_PCRASTER_MODELENGINE_PATH pcraster_model_engine LOCATION)
    ###
    ### ADD_CUSTOM_COMMAND(
    ###   OUTPUT ${_DEPLOYMENT_OUTPUT}
    ###   COMMAND ${Python_EXECUTABLE} ${DEVENV}/Scripts/checkSharedLibraryDependencies.py -d ${_DEPLOYMENT_OUTPUT} ${_PCRASTER_MODELENGINE_PATH}
    ###   COMMENT "Creating deployment.txt"
    ###   DEPENDS pcraster_model_engine
    ### )

    add_custom_target(LinkOutDoxygenDocumentation ALL
        DEPENDS
            ${_DOC_OUTPUT}
            # ${_DEPLOYMENT_OUTPUT}
    )

    ### INSTALL(FILES
    ###     ${_DEPLOYMENT_OUTPUT}
    ###   DESTINATION
    ###     doc/developer/linkout
    ### )

    install(
        DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}/html
        DESTINATION
            doc/developer/linkout
    )
endif()


### # manual
### IF(NOT WIN32 AND NOT APPLE)
###   ADD_CUSTOM_COMMAND(
###     OUTPUT ${_MANUAL_BUILD_DIR}/latex/PCRaster.pdf
###     OUTPUT ${_MANUAL_BUILD_DIR}/html/index.html
###     COMMAND make -j1 -C ${_MANUAL_DIR}/
###     DEPENDS ${_MANUAL_DIR}/manual.xml
###   )
###   ADD_CUSTOM_TARGET(PCRasterManual ALL
###     DEPENDS
###       mapattr
###       asc2map
###       pcrcalc
###       col2map
###       map2asc
###       map2col
###       table
###       # manpng
###       ${_MANUAL_BUILD_DIR}/latex/PCRaster.pdf
###       ${_MANUAL_BUILD_DIR}/html/index.html
###   )
###   INSTALL(FILES
###     ${_MANUAL_BUILD_DIR}/latex/PCRaster.pdf
###     DESTINATION
###     doc/PCRaster
###   )
###   INSTALL(DIRECTORY
###     ${_MANUAL_BUILD_DIR}/html
###     DESTINATION
###     doc/PCRaster
###   )
### ENDIF()

# # apiDocs:
# 	@doxy pcrcalc.h apiDocsMain
#
# # to make pcrcalcd.h working if living in $(OLDPCRTREE)/include
# # USE_IN_PCRCALC use internal types in pcrcalcd.h
# # PROFILE= -pg
#
# TESTPREQ=messagestest.chk
# GENSRCS=messagestest.chk
# messagestest.chk: messagestest.xml
# 	@xmlvalid messagestest.xml
# 	@touch messagestest.chk
#
# GENSRCS+=calcLibWrapper.cc
# calcLibWrapper.cc: operation.dtd CalcLibDemo.xml $(OLDPCRTREE)/devbin/calcLibWrap
# 	xmlvalid CalcLibDemo.xml
# 	xmlpretty CalcLibDemo.xml objectlinkExpanded.xml
# 	calcLibWrap objectlinkExpanded.xml
#
# clean::
# 	rm -f messagestest.chk rm calcLibWrapper.cc objectlinkExpanded.xml operationExpanded.xml
#
# # xmlpretty needs local ln
# #  if no ln (win32) -> cp , chmod read-only
# GENSRCS+=calcLibWrapper.cc
# operation.dtd: $(OLDPCRTREE)/template/xml/operation.dtd
# 	ln -f $(OLDPCRTREE)/template/xml/operation.dtd operation.dtd
# clean::
# 	rm -f operation.dtd
# ifeq ($(HOST_PLATFORM),linux)
# HAS_LEX=yes
# endif
# ifeq ($(HOST_PLATFORM),cygwin)
# HAS_LEX=yes
# endif
#
# ifdef HAS_LEX
#
# objsize.txt: $(OBJS)
# 	nm -oC --size-sort -td --radix=d $(OBJS) > objsize.txt
#
# LEX_SPAWN= lexvaluefile.cc lexnewcalc.cc
# GENSRCS+= $(LEX_SPAWN)
# # need flex >= 2.5.26 avail in debian 3.X
# # -L do NOT include #line directives
# FLEXER=flex -+
#
# lexvaluefile.cc: valuefile.l
# 	$(FLEXER) -t valuefile.l > lexvaluefile.cc
# lexnewcalc.cc: newcalc.l
# 	$(FLEXER) -t newcalc.l > lexnewcalc.cc
# endif
#
# clean::
# 	rm -f *.inc major_op.h
#
# ##############################################################
#
# # set ADEB to -gd for debugging grammar trace code
# # ADEB=-gd
# # set ALINE to -gl for #line in created files
# ALINE=-gl
# # -gx do not create lexer parth
# AFLAGS = -CC -w2 $(ALINE)  $(ADEB) -ck 2 -gx
# # -prc on -mrhoist on
# # DFLAGS = -C2 -CC -Wambiguity
# DFLAGS = -C2 -CC
#
# # create parser
# ANTLR_SPAWN = calcparser.cpp Parser.cpp \
#               Parser.h tokens.h
# GENSRCS+=$(ANTLR_SPAWN)
# $(ANTLR_SPAWN) : calcparser.g
# 	@antlr_1x $(AFLAGS) calcparser.g
#
# # sed action was to hard cast something gcc complains
# # 	@sed 's/	token_tbl =/&(ANTLRChar **)/' < Parser.cpp > Parser.cc
# # 	@rm -f Parser.cpp
#
# grammar.txt:
# 	@antlr_1x -p -CC calcparser.g > grammar.txt
#
# calcparser.o:
# 	@$(CC) $(subst -Wall,,$(CFLAGS)) -c calcparser.cpp
# Parser.o:
# 	@$(CC) $(CFLAGS) -c Parser.cpp
#
# clean::
# 	rm -f $(ANTLR_SPAWN) $(LEX_SPAWN)
#
# genForScons: $(GENSRCS)
# 	@echo '"$(GENSRCS)"';
