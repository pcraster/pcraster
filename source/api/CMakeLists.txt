add_library(api STATIC
    bootapi
    ranuint1
    ranint4
    ranreal8
    testapi
    mapdim
    ranpriv
    fieldapi_common
    fieldapi_scalardomaincheck
    fieldapi_interface
    fieldapi_templates
)

add_library(pcraster::api ALIAS api)

target_include_directories(api
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(api
    PUBLIC
        pcraster::pcrgeo
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE api
        NAMES
            fieldapi_interfacetest
            fieldapi_readonlynonspatialtest
            fieldapi_readonlyspatialtest
            fieldapi_readwritedatatest
        LINK_LIBRARIES
            pcraster::api
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
