if(PCRASTER_BUILD_TEST)
    add_subdirectory(UnitTests)
endif()

set(SOURCES
    aggregationfunctions.py
    dynamicBase.py
    dynamicFramework.py
    dynamicPCRasterBase.py
    frameworkBase.py
    generalfunctions.py
    __init__.py
    kalmanFilterFramework.py
    kfBase.py
    kfPCRasterBase.py
    mcBase.py
    mcFramework.py
    mcPCRasterBase.py
    particleFilterFramework.py
    PCRasterPythonFramework.py
    pfBase.py
    pfPCRasterBase.py
    regression.py
    staticBase.py
    staticFramework.py
    staticPCRasterBase.py
    Timeoutput.py
    ${PROJECT_SOURCE_DIR}/environment/script/forkscript.py
    ${PROJECT_SOURCE_DIR}/environment/script/pcr.py
    ${PROJECT_SOURCE_DIR}/environment/script/pcrstat.py
    ${PROJECT_SOURCE_DIR}/environment/script/shellscript.py
    ${PROJECT_SOURCE_DIR}/environment/script/utils.py
)

install(
    FILES
        ${SOURCES}
    DESTINATION
        python/pcraster/framework
)

set(FRAMEWORK_BUILD_DIRECTORY
    ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}/pcraster/framework
)

# This enables us to test and use the code as if it where installed already.
# Dummy target that does nothing. This enables us to attach post-build commands.
add_custom_target(python_modelling_framework_dummy ALL)

add_dependencies(python_modelling_framework_dummy
    _pcraster_modflow_python
)

add_custom_command(
    TARGET python_modelling_framework_dummy
    POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E remove_directory ${FRAMEWORK_BUILD_DIRECTORY}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${FRAMEWORK_BUILD_DIRECTORY}
        COMMAND ${CMAKE_COMMAND} -E copy ${SOURCES} ${FRAMEWORK_BUILD_DIRECTORY}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS ${SOURCES}
)
