#ifndef INCLUDED_DAL_EXCEPTION
#define INCLUDED_DAL_EXCEPTION



// Library headers.
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
/*!
  \todo Base this class on std::exception? See
        http://www.boost.org/more/error_handling.html.
*/
class Exception
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

  /* virtual */    ~Exception          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  std::string const& message           () const;

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



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
