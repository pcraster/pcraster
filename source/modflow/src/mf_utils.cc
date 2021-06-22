#include "mf_utils.h"

#include <filesystem>
#include <iostream>



namespace fs = std::filesystem;


namespace mf {


std::string execution_path(const std::string& directory, const std::string& filename){

  // No subdirectory -> write to current working directory
  if(directory.empty()){
    return filename;
  }

  fs::path p{directory};
  fs::file_status s = fs::status(p);

  if(fs::is_directory(s) == false){
    throw std::runtime_error("Can not write file '" + filename + "', directory '" + directory + "' does not exist\n");
  }

  p /= filename;

  return p.string();
}

}
