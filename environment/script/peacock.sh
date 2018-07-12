#!/usr/bin/env bash
set -e


function print_usage()
{
    echo -e "\
usage: $0 [-h] <download_dir> <prefix> <source>

-h              Show (this) usage information

download_dir    Directory to store downloaded files
prefix          Directory to install the resulting files
source          Directory of Peacock sources

Additional CMake arguments can be passed by setting the variable
CMAKE_ARGUMENTS before calling this script, e.g.:
$ CMAKE_ARGUMENTS="-DPython_ADDITIONAL_VERSIONS=3" peacock.sh"
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

    if [[ $hostname == "gransasso" ]]; then
        # Ubuntu 16.04
        skip_build_boost=1
        skip_build_qt=1
        skip_build_qwt=1
    elif [[ $hostname == "sonic.geo.uu.nl" ]]; then
        # CentOS 7
        skip_build_qt=1
    elif [[
            $hostname == "triklav" ||
            $hostname == "triklav.local" ||
            $hostname == "triklav.soliscom.uu.nl"
            ]]; then
        # macOS 10.12.1 Sierra / macports
        skip_build_boost=1
        skip_build_gdal=1
        skip_build_qt=1
        # Bug in 'port install qwt61 +qt5' (seen on 20161205), so build
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
    # KDJ: In case Python headers are not found, try this:
    #    CPLUS_INCLUDE_PATH=<path_to_headers> peacock.sh ...
    if [[ ! -v skip_build_boost || $skip_build_boost != 1 ]]; then
        options+=("-Dbuild_boost=true")
        options+=("-Dboost_version=1.57.0")
        options+=("-Dboost_build_boost_date_time=true")
        options+=("-Dboost_build_boost_filesystem=true")
        options+=("-Dboost_build_boost_math=true")
        options+=("-Dboost_build_boost_program_options=true")
        options+=("-Dboost_build_boost_python=true")
        options+=("-Dboost_build_boost_system=true")
        options+=("-Dboost_build_boost_test=true")
        options+=("-Dboost_build_boost_timer=true")
    fi


    # GDAL.
    if [[ ! -v skip_build_gdal || $skip_build_gdal != 1 ]]; then
        `python -c "import gdal"` >/dev/null 2>&1 && \
            skip_build_gdal_python_package=1

        options+=("-Dbuild_gdal=true")
        options+=("-Dgdal_version=2.0.1")
        if [[ ! -v skip_build_gdal_python_package || \
                $skip_build_gdal_python_package != 1 ]]; then
            options+=("-Dgdal_build_python_package=true")
        fi
    fi


    # Qt
    if [[ ! -v skip_build_qt || $skip_build_qt != 1 ]]; then
        options+=("-Dbuild_qt=true")
        # TODO Not supported yet: https://github.com/geoneric/peacock/issues/53
        options+=("-Dqt_version=5.7.1")
    fi


    # Qwt
    if [[ ! -v skip_build_qwt || $skip_build_qwt != 1 ]]; then
        options+=("-Dbuild_qwt=true")
        options+=("-Dqwt_version=6.1.2")
    fi


    # printf '%s\n' "${options[@]}"
    cmake $CMAKE_ARGUMENTS "${options[@]}" $source
    cmake --build . --target all
}


parse_commandline $*
build_software
