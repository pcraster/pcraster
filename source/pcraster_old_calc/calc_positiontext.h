#ifndef INCLUDED_CALC_POSITIONTEXT
#define INCLUDED_CALC_POSITIONTEXT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_POSITION
#include "calc_position.h"
#define INCLUDED_CALC_POSITION
#endif



namespace calc {
  // PositionText declarations.
}



namespace calc {



//! Position defined by line and char (column) number in a named Ascii text file
class PositionText : public Position
{
  typedef boost::shared_ptr<std::string> StringSharedPtr;

  StringSharedPtr d_fileName;

  //! line in script, 1-based
  int   d_lineNr;
  //! char position on that line, 1-based
  int   d_charNr;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  PositionText&           operator=           (const PositionText&);


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PositionText(
                       StringSharedPtr fileName,
                       int lineNr,int charNr);

                   PositionText              ();

  /* virtual */   ~PositionText              ();
                   PositionText              (const PositionText&);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void           throwError(const std::string& msg) const;

  PositionText*  createClone() const;

  std::string    text() const;

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
