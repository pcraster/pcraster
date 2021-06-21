#ifndef INCLUDED_DEV_FILESYSTEMUTILS
#define INCLUDED_DEV_FILESYSTEMUTILS

#include <filesystem>





namespace dev {

std::filesystem::path pathToExecutable (std::filesystem::path const& path);

std::filesystem::path pathToPythonExtension(
                                        std::filesystem::path const& path);

std::filesystem::path prefix           (std::filesystem::path const& path);

} // namespace dev

#endif
