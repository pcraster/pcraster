#ifndef INCLUDED_DAL_EXCEPTION
#define INCLUDED_DAL_EXCEPTION

#include "dal_Configure.h"

#include <exception>
#include <string>



namespace dal {
  // Exception declarations.
}



namespace dal {


#ifdef _MSC_VER
#    pragma warning(push)
#    pragma warning(disable : 4275 4251)
//   4275: An exported class was derived from a class that wasn't exported.
//   4251: class 'type1' needs to have dll-interface to be used by clients of class 'type2'
//   Can be ignored if your class is derived from a type in the C++ Standard Library
#endif

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

#ifdef _MSC_VER
#    pragma warning(pop)
#endif

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
