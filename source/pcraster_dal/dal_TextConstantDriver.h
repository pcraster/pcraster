#ifndef INCLUDED_DAL_TEXTCONSTANTDRIVER
#define INCLUDED_DAL_TEXTCONSTANTDRIVER



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_CONSTANTDRIVER
#include "dal_ConstantDriver.h"
#define INCLUDED_DAL_CONSTANTDRIVER
#endif

#ifndef INCLUDED_DAL_TEXTFILEDRIVER
#include "dal_TextFileDriver.h"
#define INCLUDED_DAL_TEXTFILEDRIVER
#endif



namespace dal {
  // TextConstantDriver declarations.
}



namespace dal {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class TextConstantDriver: public ConstantDriver,
                          public TextFileDriver
{

  friend class TextConstantDriverTest;

private:

  bool             open                (Constant& constant,
                                        std::istream& stream) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TextConstantDriver  ();

  /* virtual */    ~TextConstantDriver ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Constant*        open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             read                (Constant& constant,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Constant*         read               (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

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
