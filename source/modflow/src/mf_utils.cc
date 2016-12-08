#include "mf_utils.h"

#include <boost/filesystem.hpp>
#include <iostream>



namespace bfs = boost::filesystem;


namespace mf {


std::string execution_path(const std::string& directory, const std::string& filename){

  // No subdirectory -> write to current working directory
  if(directory.empty()){
    return filename;
  }

  bfs::path p{directory};
  bfs::file_status s = bfs::status(p);

  if(bfs::is_directory(s) == false){
    throw std::runtime_error("Can not write file '" + filename + "', directory '" + directory + "' does not exist\n");
  }

  p /= filename;

  return p.string();
}

}
