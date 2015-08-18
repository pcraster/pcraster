#ifndef INCLUDED_CALC_POSITIONNAME
#define INCLUDED_CALC_POSITIONNAME



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
  typedef boost::shared_ptr<std::string> StringSharedPtr;

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

  /* virtual */   ~PositionName              ();
                   PositionName              (const PositionName&);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void           throwError                  (const std::string& msg) const;

  PositionName*  createClone                 () const;

  std::string    fullText                    () const;
  std::string    shortText                   () const;

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
