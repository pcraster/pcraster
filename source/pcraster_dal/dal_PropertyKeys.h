#ifndef INCLUDED_DAL_PROPERTYKEYS
#define INCLUDED_DAL_PROPERTYKEYS

/*
 * A namespaced convention is used to make keys
 * more unique. The dal namespace is used for general
 * applicable keys
 */

// used for csf maps
#define DAL_CSF_VALUESCALE             "csf::ValueScale"
#define DAL_CSF_PROJECTION             "csf::Projection"
#define DAL_CSF_ANGLE                  "csf::Angle"

// many formats can write a legend
#define DAL_LEGEND                     "dal::Legend"

// Many formats are file based and don't natively support all dimensions.
#define DAL_FILENAME_CONVENTION        "dal::FilenameConvention"

#define DAL_DEFAULT_EXTENSION          "dal::DefaultExtension"

#define DAL_FEATURE_DRIVER_PARSE_STRATEGY "dal::FeatureDriverParseStrategy"

// Driver properties all start with dal::Driver.
#define DAL_DRIVER_GENERAL             "dal::DriverGeneral"

// example not used, write EHdr integer as 1 or 4 bit
#define DAL_EHDR_NBITS                 "dal::ehdr::nbits"

#endif
