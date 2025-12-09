// file      : xsd/cxx/version.hxx.in
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef LIBXSD_VERSION // Note: using the version macro itself.

// The numeric version format is AAAAABBBBBCCCCCDDDE where:
//
// AAAAA - major version number
// BBBBB - minor version number
// CCCCC - bugfix version number
// DDD   - alpha / beta (DDD + 500) version number
// E     - final (0) / snapshot (1)
//
// When DDDE is not 0, 1 is subtracted from AAAAABBBBBCCCCC. For example:
//
// Version      AAAAABBBBBCCCCCDDDE
//
// 0.1.0        0000000001000000000
// 0.1.2        0000000001000020000
// 1.2.3        0000100002000030000
// 2.2.0-a.1    0000200001999990010
// 3.0.0-b.2    0000299999999995020
// 2.2.0-a.1.z  0000200001999990011
//
#define LIBXSD_VERSION       400002000000000ULL
#define LIBXSD_VERSION_STR   "4.2.0"
#define LIBXSD_VERSION_ID    "4.2.0"
#define LIBXSD_VERSION_FULL  "4.2.0"

#define LIBXSD_VERSION_MAJOR 4
#define LIBXSD_VERSION_MINOR 2
#define LIBXSD_VERSION_PATCH 0

#define LIBXSD_PRE_RELEASE   false

#define LIBXSD_SNAPSHOT      0ULL
#define LIBXSD_SNAPSHOT_ID   ""

// Note that Xerces and Expat compatibility is verified by the respective
// parsers (see parser/xerces/elements.hxx and parser/expat/elements.hxx for
// details).

#endif // LIBXSD_VERSION
