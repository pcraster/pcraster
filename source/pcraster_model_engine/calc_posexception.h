#ifndef INCLUDED_CALC_POSEXCEPTION
#define INCLUDED_CALC_POSEXCEPTION

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

// Module headers.

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



namespace calc {
  // PosException declarations.
}



namespace calc {



//! exception containing positional (vi-friendly) information
/*! all ctors do create an understandable com::Exception object,
 *  while information can be duplicated here for even better messages
 */
class PCR_DLL_CLASS PosException : public com::Exception
{

private:
  //  Assignment operator. DEFAULT
  // PosException&           operator=           (const PosException&);

  //  Copy constructor. DEFAULT
  //               PosException               (const PosException&);

  //! Default constructor. NOT IMPLEMENTED.
                   PosException               ();

  //! error description
  //! textual position description
  std::string      d_message;
  std::string      d_position;

  void finish(std::ostringstream& s);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
   PosException(
     const std::string& message);

   PosException(
     const std::string& pos,
     const std::string& message,
     bool  positionFirst);

  /* virtual */  ~PosException              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const std::string& message             () const;
  const std::string& position            () const;
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
