#!/usr/bin/env bash
set -e


function print_usage()
{
    echo -e "\
usage: $0 [-h] <download_dir> <prefix> <source>

-h              Show (this) usage information.

download_dir    Directory to store downloaded files.
prefix          Directory to install the resulting files.
source          Directory of Peacock sources."
}


function parse_commandline()
{
    while getopts h option; do
        case $option in
            h) print_usage; exit 0;;
            *) print_usage; exit 2;;
        esac
    done
    shift $((OPTIND-1))

    if [ $# -ne 3 ]; then
        print_usage
        exit 2
    fi

    download_dir=$1
    prefix=$2
    source=$3
}


function build_software()
{
    # Prefer version of libraries installed by the package manager. In
    # case the correct (major) version is not available, we can build one.
    local hostname=`hostname`

    if [[ $hostname == "sonic.geo.uu.nl" ]]; then
        # CentOS 7
        skip_build_qt=1
    fi

    if [[ $hostname == "gransasso" ]]; then
        # Ubuntu 16.04
        skip_build_boost=1
        skip_build_qt=1
        skip_build_qwt=1
    fi


    if [[ $OSTYPE == "cygwin" ]]; then
        options+=("-GUnix Makefiles")
        options+=("-DCMAKE_MAKE_PROGRAM=mingw32-make")
    fi

    options+=("-Dpeacock_download_dir=$download_dir")
    options+=("-Dpeacock_prefix=$prefix")
    options+=("-DCMAKE_VERBOSE_MAKEFILE=OFF")


    # Boost.
    if [ ! "$skip_build_boost" ]; then
        options+=("-Dbuild_boost=true")
        options+=("-Dboost_version=1.57.0")
        options+=("-Dboost_build_boost_date_time=true")
        options+=("-Dboost_build_boost_filesystem=true")
        options+=("-Dboost_build_boost_math=true")
        options+=("-Dboost_build_boost_program_options=true")
        options+=("-Dboost_build_boost_python=true")
        options+=("-Dboost_build_boost_regex=true")
        options+=("-Dboost_build_boost_system=true")
        options+=("-Dboost_build_boost_test=true")
        options+=("-Dboost_build_boost_timer=true")
    fi


    # GDAL.
    if [ ! "$skip_build_gdal" ]; then
        `python -c "import gdal"` >/dev/null 2>&1 && \
            skip_build_gdal_python_package=1

        options+=("-Dbuild_gdal=true")
        options+=("-Dgdal_version=2.0.1")
        if [ ! "$skip_build_gdal_python_package" ]; then
            options+=("-Dgdal_build_python_package=true")
        fi
    fi


    # Qt
    if [ ! "$skip_build_qt" ]; then
        options+=("-Dbuild_qt=true")
        # TODO Not supported yet: https://github.com/geoneric/peacock/issues/53
        options+=("-Dqt_version=5.6.2")
    fi


    # Qwt
    if [ ! "$skip_build_qwt" ]; then
        options+=("-Dbuild_qwt=true")
        options+=("-Dqwt_version=6.1.2")
    fi


    # PCRaster raster format.
    options+=("-Dbuild_pcraster_raster_format=true")
    options+=("-Dpcraster_raster_format_version=1.3.1")


    # Fern.
    options+=("-Dbuild_fern=true")
    options+=("-Dfern_git_repository=https://github.com/geoneric/fern.git")
    options+=("-Dfern_git_tag=f1114c8ccd2629686e38eee8059b39477e428bc6")
    options+=("-Dfern_build_fern_algorithm=true")
    options+=("-Dfern_build_fern_documentation=true")
    options+=("-Dfern_build_fern_test=true")


    cmake "${options[@]}" $source
    cmake --build . --target all
}


parse_commandline $*
build_software
