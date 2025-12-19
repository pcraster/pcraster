#ifndef INCLUDED_CALC_POINTCODEBLOCKDLL
#define INCLUDED_CALC_POINTCODEBLOCKDLL

#include "stddefx.h"

#include <vector>



namespace com {
 class DynamicLibrary;

}



namespace calc {

class PointCodeBlock;

/*! \short Create dynamic library (dll) source, code, compile it and set
 *         the function address to the PointCodeBlock's
 */
class PointCodeBlockDll
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  PointCodeBlockDll&           operator=           (PointCodeBlockDll const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   PointCodeBlockDll               (PointCodeBlockDll const& rhs);

  typedef std::vector<PointCodeBlock *>  Blocks;

  com::DynamicLibrary*    d_dll{nullptr};

  void             load                            (const Blocks& l);

  void             generateSource                  (const Blocks& l) const;
  void             compile                         ()                const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PointCodeBlockDll               (const Blocks& l);

  /* virtual */    ~PointCodeBlockDll              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             unload                          ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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



} // namespace calc

#endif
