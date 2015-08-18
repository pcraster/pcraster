include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
)

SET(TEST_SOURCES
    mldd_test.cc
    mldd_dagrastertest.cc
    mldd_outedgeiteratortest.cc
    mldd_vertexiteratortest.cc
)
ADD_EXECUTABLE(test_mldd
    ${TEST_SOURCES}
)
# the linkin libraries used
ADD_DEPENDENCIES(test_mldd example1 example2 exampleCpp)
# SET(_LINK_LIBRARIES
#     mldd_static
#     pcrgeo
#     misc
#     ${PCRASTER_RASTERFORMAT_LIBRARY}
# )
# IF(WIN32)
#     TARGET_LINK_LIBRARIES(test_mldd
#         ${_LINK_LIBRARIES}
#         ${QT_QTCORE_LIBRARIES}
#     )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(test_mldd
        mldd # _static
        # ${_LINK_LIBRARIES}
    )
# ENDIF(WIN32)

ADD_CUSTOM_TARGET(run_test_tests
    COMMAND test_mldd
    DEPENDS test_mldd
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_tests)
