#ifndef INCLUDED_COM_UNIQUESTRINGGENERATOR
#define INCLUDED_COM_UNIQUESTRINGGENERATOR

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  // UniqueStringGenerator declarations.
}



namespace com {


//! maintain a set of strings and generate a unique string not yet in set
/*!
 * To get new strings like NewFile-1, NewFile-2, ..etc.. where these strings are garantueed
 * to be unique in a certain collection <i>C</i> one does something like:
 * \code
 *  com::UniqueStringGenerator g;
 *  // pseudo code, for 
 *  for a in all strings in C
 *    g.insert(a);
 *  g.setPrefix("NewFile-");
 *  std::cout << g.generate() << "\n";
 * \endcode
 */
class UniqueStringGenerator: public std::set<std::string>
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  UniqueStringGenerator&           operator=           (const UniqueStringGenerator&);

  //! Copy constructor. NOT IMPLEMENTED.
                   UniqueStringGenerator               (const UniqueStringGenerator&);

  //! prefix for unique string to be generated
  std::string d_prefix;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   UniqueStringGenerator               ();

  /* virtual */    ~UniqueStringGenerator              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  std::string generate();
  void setPrefix(const std::string& prefix);


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



} // namespace com

#endif
