#ifndef INCLUDED_CALC_POSEXCEPTION
#define INCLUDED_CALC_POSEXCEPTION

#include "stddefx.h"
#include "com_exception.h"
#include "pcraster_model_engine_export.h"

#include <string>



namespace calc {
  // PosException declarations.
}



namespace calc {



//! exception containing positional (vi-friendly) information
/*! all ctors do create an understandable com::Exception object,
 *  while information can be duplicated here for even better messages
 */
class PCR_ME_EXPORT PosException : public com::Exception
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

  /* virtual */  ~PosException              () override;

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
