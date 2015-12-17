include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
)

SET(SOURCES
    bootapi.c
    ranuint1.c
    ranint4.c
    ranreal8.c
    testapi.c
    mapdim.c
    ranpriv.c
    fieldapi_common.cc
    fieldapi_scalardomaincheck.cc
    fieldapi_interface.cc
    fieldapi_templates.cc
)
ADD_LIBRARY(api
    ${SOURCES}
)


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        fieldapi_interfacetest
        fieldapi_readonlynonspatialtest
        fieldapi_readonlyspatialtest
        fieldapi_readwritedatatest
    )
    add_unit_tests(
        SCOPE api
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            api
            pcrgeo
    )
endif()




# NAMESPACE=fieldapi
# now in CVS
# clean::
# 	rm -f ranuint1.c ranint4.c ranreal8.c testapi.inc api.h
# 
# 
# ranuint1.c: rancom.tem
# 	@$(ECHO) "/* DO NOT EDIT: CREATED FROM rancom.tem */" > ranuint1.c
# 	@$(ECHO) "#line 1 \"rancom.tem\"" >> ranuint1.c
# 	@sed -e s/THIS_TYPE_T/UINT1_T/g  -e s/THIS_TYPE/UINT1/g rancom.tem >> ranuint1.c
# 
# ranint4.c: rancom.tem
# 	@$(ECHO) "/* DO NOT EDIT: CREATED FROM rancom.tem */" > ranint4.c
# 	@$(ECHO) "#line 1 \"rancom.tem\"" >> ranint4.c
# 	@sed -e s/THIS_TYPE_T/INT4/g  -e s/THIS_TYPE/INT4/g rancom.tem >> ranint4.c
# 
# ranreal8.c: rancom.tem
# 	@$(ECHO) "/* DO NOT EDIT: CREATED FROM rancom.tem */" > ranreal8.c
# 	@$(ECHO) "#line 1 \"rancom.tem\"" >> ranreal8.c
# 	@sed -e s/THIS_TYPE_T/REAL8/g  -e s/THIS_TYPE/REAL8/g rancom.tem >> ranreal8.c
# 
# rancom.tem: api.h
# 
# api.h: randef.tem randef.hh Makefile
# 	@cat randef.hh > api.h
# 	@$(ECHO) "#line 1 \"randef.tem\"" >> api.h
# 	@sed -e s/THIS_TYPE_T/UINT1_T/g  -e s/THIS_TYPE/UINT1/g randef.tem >> api.h
# 	@$(ECHO) "#line 1 \"randef.tem\"" >> api.h
# 	@sed -e s/THIS_TYPE_T/INT4/g  -e s/THIS_TYPE/INT4/g randef.tem >> api.h
# 	@$(ECHO) "#line 1 \"randef.tem\"" >> api.h
# 	@sed -e s/THIS_TYPE_T/REAL8/g  -e s/THIS_TYPE/REAL8/g randef.tem >> api.h
# 	@$(ECHO) "#ifdef __cplusplus" >> api.h
# 	@$(ECHO) "}" >> api.h
# 	@$(ECHO) "#endif" >> api.h
# 	@$(ECHO) "#endif /* API__H */" >> api.h
# 
# testapi.c: testapi.inc
# 
# testapi.inc: testapi.tem
# 	@$(ECHO) "/* DO NOT EDIT: CREATED FROM testapi.tem */" > testapi.inc
# 	@$(ECHO) "#line 1 \"testapi.tem\"" >> testapi.inc
# 	@sed 's/THIS_TYPE/UINT1/g' < testapi.tem >> testapi.inc 
# 	@$(ECHO) "#line 1 \"testapi.tem\"" >> testapi.inc
# 	@sed 's/THIS_TYPE/INT4/g' < testapi.tem >> testapi.inc 
# 	@$(ECHO) "#line 1 \"testapi.tem\"" >> testapi.inc
# 	@sed 's/THIS_TYPE/REAL8/g' < testapi.tem >> testapi.inc 
