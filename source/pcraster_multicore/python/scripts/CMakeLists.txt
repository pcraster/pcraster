configure_file(__init__.py.in
    ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/__init__.py
    COPYONLY
)

configure_file(operations.py.in
    ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/_operations.py
    COPYONLY
)

configure_file(operators.py.in
    ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/_operators.py
    COPYONLY
)
