#ifndef INCLUDED_DEV_FILESYSTEMUTILS
#define INCLUDED_DEV_FILESYSTEMUTILS

// External headers.
#ifndef INCLUDED_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>
#define INCLUDED_BOOST_FILESYSTEM
#endif

// Project headers.

// Module headers.




namespace dev {

boost::filesystem::path pathToExecutable(
                                        boost::filesystem::path const& path);

boost::filesystem::path pathToPythonExtension(
                                        boost::filesystem::path const& path);

boost::filesystem::path prefix         (boost::filesystem::path const& path);

} // namespace dev

#endif
