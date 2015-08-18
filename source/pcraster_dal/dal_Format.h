#ifndef INCLUDED_DAL_FORMAT
#define INCLUDED_DAL_FORMAT



// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_COMPILER
#include "dev_Compiler.h"
#define INCLUDED_DEV_COMPILER
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif



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

  DatasetType      d_datasetType;

  std::vector<std::string> d_extensions;

  StorageMedium    d_medium;

  Discretisation   d_discretisation;

  Type             d_type;

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
