#ifndef INCLUDED_DAL_FORMAT
#define INCLUDED_DAL_FORMAT

#include "dev_Compiler.h"
#include "dal_Configure.h"
#include "dal_Def.h"

#include <string>
#include <vector>


namespace dal {
  // Format declarations.
}



namespace dal {



//! This class is for objects containing meta information about data formats.
/*!
  Different data formats have different ways of storing data: files, databases,
  memory, etc.

  \todo What about loosing the discretisation info?
*/
class PCR_DAL_DECL Format
{

  friend class FormatTest;

public:

  enum StorageMedium {
    Database,
    File,
    Memory,
    NrMedia
  };

  enum Discretisation {
    Raster,
    Vector,
    Block,
    NrDiscretisations
  };

  enum Type {
    Attribute,
    Graphics,
    NrTypes
  };

private:

  std::string      d_name;

  std::string      d_description;

  DatasetType      d_datasetType{NR_DATASET_TYPES};

  std::vector<std::string> d_extensions;

  StorageMedium    d_medium{NrMedia};

  Discretisation   d_discretisation{NrDiscretisations};

  Type             d_type{NrTypes};

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Format              ();

                   Format              (std::string const& name,
                                        std::string const& description,
                                        DatasetType datasetType,
                                        StorageMedium medium);

                   Format              (std::string const& name,
                                        std::string const& description,
                                        DatasetType datasetType,
                                        StorageMedium medium,
                                        Type type);

                   Format              (std::string const& name,
                                        std::string const& description,
                                        DatasetType datasetType,
                                        StorageMedium medium,
                                        Discretisation discretisation,
                                        Type type);

                   Format              (std::string const& name,
                                        std::string const& description,
                                        DatasetType datasetType,
                                        std::vector<std::string> const& extensions,
                                        Discretisation discretisation,
                                        Type type);

  /* virtual */    ~Format             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setExtensions       (std::vector<std::string> const& extensions);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  std::string const& name              () const;

  std::string const& description       () const;

  DatasetType      datasetType         () const;

  bool             isFileBased         () const;

  bool             isForGraphics       () const;

  bool             isForRasterGraphics () const;

  std::string const& extension         () const;

  std::vector<std::string> const& extensions() const;

  bool             extensionMatches    (std::string const& extension) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
