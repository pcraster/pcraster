#ifndef INCLUDED_CALC_POSITIONTEXT
#define INCLUDED_CALC_POSITIONTEXT

#include "stddefx.h"
#include "calc_position.h"

#include <memory>


namespace calc {
  // PositionText declarations.
}



namespace calc {



//! Position defined by line and char (column) number in a named Ascii text file
class PositionText : public Position
{
  typedef std::shared_ptr<std::string> StringSharedPtr;

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

  /* virtual */   ~PositionText              () override;
                   PositionText              (const PositionText&);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void           throwError(const std::string& msg) const override;

  PositionText*  createClone() const override;

  std::string    text() const override;

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
