#ifndef INCLUDED_OLDCALC_POSITIONNAME
#define INCLUDED_OLDCALC_POSITIONNAME

#include "stddefx.h"
#include "calc_position.h"

#include <memory>


namespace calc {
  // PositionName declarations.
}



namespace calc {



//! Position defined by a single name
/*!
 * Most commonly used if the name denotes a file but the file position
 * is irrelevant for the user (xml file)
 */
class PositionName : public Position
{
  typedef std::shared_ptr<std::string> StringSharedPtr;

  StringSharedPtr d_name;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  PositionName&           operator=           (const PositionName&);


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PositionName(StringSharedPtr name);

                   PositionName              ();

  /* virtual */   ~PositionName              () override;
                   PositionName              (const PositionName&);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void           throwError(const std::string& msg) const override;

  PositionName*  createClone() const override;

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
