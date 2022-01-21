#ifndef INCLUDED_DAL_EXCEPTION
#define INCLUDED_DAL_EXCEPTION

#include "dal_Configure.h"

// Library headers.
#include <exception>

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



// PCRaster library headers.

// Module headers.



namespace dal {
  // Exception declarations.
}



namespace dal {



//! Base exception class for the DAL library.
class PCR_DAL_DECL Exception : public virtual std::exception
{

  friend class ExceptionTest;

private:

  //! Message.
  std::string      d_message;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Exception           (std::string const& message);

  /* virtual */    ~Exception          () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  std::string const& message           () const;

  const char*      what                () const noexcept override;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     message Error message.
*/
inline Exception::Exception(std::string const& message)
  : d_message(message)
{
}

//! Destructor.
/*!
*/
inline Exception::~Exception()
{
}

//! Returns the message.
/*!
  \return    Message.
*/
inline std::string const& Exception::message() const
{
  return d_message;
}

//! Returns the message.
/*!
  \return    Message.
*/
inline const char* Exception::what() const noexcept
{
  return d_message.c_str();
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
