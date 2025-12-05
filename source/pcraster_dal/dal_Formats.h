#ifndef INCLUDED_DAL_FORMATS
#define INCLUDED_DAL_FORMATS

#include "dal_Configure.h"
#include "dal_Format.h"



namespace dal {
  // Formats declarations.
}



namespace dal {



//! Container class for Format objects.
/*!
  A Formats object can be created for a specific application or for different
  functions of an applications (Formats object for read formats, Formats object
  for write formats, for example).
*/
class PCR_DAL_DECL Formats: public std::vector<Format>
{

  friend class FormatsTest;

private:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //                  Formats             ();

  //                  Formats             (Format const& format);

  //                  Formats             (std::vector<Format> const& formats);

  // /* virtual */    ~Formats            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  // void             add                 (Format const& format);

  // void             add                 (std::vector<Format> const& formats);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  // bool             size                () const;

  // bool             isEmpty             () const;

  Format const*    formatByExtension   (std::string const& extension) const;

  Format const*    formatByName        (std::string const& name) const;

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
