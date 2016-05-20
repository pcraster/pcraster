#ifndef INCLUDED_CALC_POINTCODEBLOCKDLL
#define INCLUDED_CALC_POINTCODEBLOCKDLL



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.



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

  com::DynamicLibrary*    d_dll;

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
