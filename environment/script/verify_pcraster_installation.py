#!/usr/bin/env python
"""
Verify that a PCRaster installation is likely to be correct.
"""
import argparse
import os
import sys
import traceback
import devbase
# sys.path.append(os.path.join(devenv.native_path_name(os.environ["AGUILA"]),
#     "environment", "scripts"))
# import verify_aguila_installation


executable_path_names = [
    os.path.join("bin", name) for name in [
        "aguila",
        "asc2map",
        "col2map",
        "legend",
        "map2asc",
        "map2col",
        "mapattr",
        "oldcalc",
        "pcrcalc",
        "mf2005",
        "resample",
        "table"
    ]
]

if sys.platform == "win32":
    executable_path_names = ["{}.exe".format(path_name) for path_name in
        executable_path_names]

required_root_directory_names = [
    "bin", "doc", "include", "lib", "python", "share"
]

required_root_file_names =[
]

required_directory_path_names=[
    # "doc/demo"
    # Developer  manual  PCRasterModflow  PCRasterPython  PCRasterPythonFramework
    "bin",
    "doc",
    # TODO "doc/aguila",
    "doc/developer",
    "doc/developer/c",
    "doc/developer/c/include",
    "doc/developer/linkout",
    "doc/developer/linkout/csharp",
    "doc/developer/xsd",
    # TODO "doc/manual",
    # TODO "doc/modflow",
    # TODO "doc/python",
    # TODO "doc/python/pcraster",
    # TODO "doc/python/pcraster/framework",
    # TODO "doc/python/pcraster/arrayed_variables",
    # TODO "doc/pcraster",
    "lib",
    "python",
    "python/pcraster",
    "python/pcraster/collection",
    "python/pcraster/framework",
    "python/pcraster/moc",
    "python/pcraster/mldd",
    "share",
    "share/gdal",
    # TODO "share/aguila/demos",
    # TODO "share/aguila/demos/xml",
] ### + verify_aguila_installation.required_directory_path_names

if sys.platform == "win32":
    required_file_path_names = [ "bin/pcraster_modflow.xml" ]
else:
    required_file_path_names = [ "lib/pcraster_modflow.xml" ]

required_file_path_names = required_file_path_names + \
    [os.path.join("doc", name, "index.html") for name in [
        # TODO "manual",
        # TODO "modflow"
    ]] + \
    [
        # TODO "doc/aguila/index.html",
        "doc/developer/c/include/pcrcalc.h",
        "doc/developer/c/include/pcrdll.h",
        # "doc/developer/linkout/deployment.txt",
        # TODO "doc/developer/linkout/LinkOutAPIUserManual.pdf",
        # TODO "doc/developer/linkout/html/index.html",
        "doc/developer/xsd/PCRaster.xsd",
        "doc/developer/xsd/commonTypes.xsd",
        # TODO "doc/python/pcraster/index.html",
        # TODO "doc/python/pcraster/arrayed_variables/index.html",
        # TODO "doc/python/pcraster/framework/index.html",
        "python/pcraster/collection/__init__.py",
        "python/pcraster/framework/__init__.py"
    ] + \
    executable_path_names + \
    [os.path.join("python", "pcraster", name) for name in [
        "__init__.py",
        "aguila.py",
        "framework/__init__.py"
    ]] + \
    [
        # TODO "share/aguila/Aguila.xsd",
        # TODO "share/aguila/demos/xml/example1.xml",
        # TODO "share/pcraster/CHANGES.TXT",
        # TODO "share/pcraster/COPYING.TXT",
        # TODO "share/pcraster/INSTALL.TXT",
        # TODO "share/gdal/LICENSE.TXT"
    ] ### + \
    ### verify_aguila_installation.required_file_path_names

# TODO
# shared_library_path_names = [
#     "pcraster_modflow"
# ]


def verify_installation(
        prefix):
    devbase.verify_package(prefix=prefix,
        required_root_directory_names=required_root_directory_names,
        required_root_file_names=required_root_file_names,
        required_directory_path_names=required_directory_path_names,
        required_file_path_names=required_file_path_names,
        executable_path_names=executable_path_names,
        python_package_directory_name="python",
        python_package_names=[
            "pcraster",
            # TODO "pcraster.collection",
            "pcraster.framework",
            # TODO "pcraster.mldd",
            # TODO "pcraster.moc",
        ])

    if sys.platform == "win32":
        # TODO
        assert not os.path.exists(os.path.join(prefix, "bin", "python27.dll"))

    # TODO On Windows, make sure import libs are not installed.


    # TODO Add support for verifying snippets:
    #   # Failed on Linux because we used to ship libz.
    #   from pcraster import *
    #   import matplotlib.pyplot as plt
    #   plt.figure().savefig("test.pdf", format="pdf")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=
        "Verify that an installation is likely to be correct")
    parser.add_argument("prefix", help="Path to installation")
    arguments = parser.parse_args()
    prefix = os.path.abspath(arguments.prefix)

    try:
        verify_installation(prefix)
        result = 0
    except:
        traceback.print_exc(file=sys.stderr)
        result = 1
    sys.exit(result)
