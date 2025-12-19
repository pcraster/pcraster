#ifndef INCLUDED_CALC_POSITIONNAME
#define INCLUDED_CALC_POSITIONNAME

#include "stddefx.h"
#include "calc_position.h"

#include  <memory>


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

                   PositionName(const std::string& name);

                   PositionName              ();

  /* virtual */   ~PositionName              () override;
                   PositionName              (const PositionName&);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void           throwError                  (const std::string& msg) const override;

  PositionName*  createClone                 () const override;

  std::string    fullText                    () const override;
  std::string    shortText                   () const override;

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
