#ifndef INCLUDED_CALC_ID
#define INCLUDED_CALC_ID



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.

// Module headers.



namespace calc {
  // Id declarations.
  class Position;
}



namespace calc {



//! An identifier with a name and position as parsed
/*!
 *  For identifier outside the AST or used to initialize ASTId type objects
 */
class Id
{

private:
  std::string      d_name;
  //! always non-zero except if empty() is true
  Position*        d_position;

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  Id&           operator=           (const Id& rhs);

  //! Copy constructor.
                   Id               (const Id& rhs);

                   Id               ();


                   Id               (const std::string& name,
                                     const Position *position);

  /* virtual */    ~Id              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setName             (const std::string& name);
  void             setPosition         (const Position* position);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //! returns name()
  const std::string            operator()() const {
    return name();
  }

  std::string        qName               () const;
  const std::string& name                () const;
        Position*    position            () const;
  bool               empty               () const;

  void               posError            (const std::string& msg) const;
  void               posError            (const std::ostringstream& msg) const;
  void               symError            (const std::string& msg) const;

  std::string        shortPosText        () const;

};


//! temporary id's with no position
/*!
 * These Id's are never exposed to the user, and have a short lifespan;
 *  e.g. search criteria or as in void calc::ParserTest::testExternalBindings().
 */
class TmpId : public Id {
  public:
    TmpId(const std::string& name);
};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool operator<(const Id& lhs, const Id& rhs);
bool operator==(const Id& lhs, const Id& rhs);
inline bool operator!=(const Id& lhs, const Id& rhs) {
  return !(lhs==rhs);
}

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
