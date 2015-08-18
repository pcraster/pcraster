#ifndef INCLUDED_CALC_ELEMENT
#define INCLUDED_CALC_ELEMENT


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
#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif

namespace calc {
  // Element declarations.
}



namespace calc {

class IScript;
class Position;

//! element of a script
/*!  An element is either a single element, such as
 *  as an id or a compound element, such as a statement
 */
class Element
{

private:
  //! script it belongs to
  IScript *d_script;

  //! position it has
  Position *d_pos;


protected:
  //! return script it belongs to
  IScript& script();

  //! only for the sake of default Symbol ctor
           Element               ();
public:
  //!
  /*!
   * \todo
   *   throwing SyntaxErrorBug is a hack to substitute good error
   *   checking in the parser
   */
  
  struct SyntaxErrorBug{};

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                     Element             (const Element& e);
  Element&  operator=                    (const Element&);
                     Element             (IScript *script, const Position *pos);


  virtual           ~Element              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //! return script it belongs to
  const IScript&             scriptConst    () const;

  void                       runtimeError   (const std::string& inMsg) const;

  virtual void               posError       (const std::string& msg) const;
  virtual void               posError       (const std::ostringstream& msg) const;

  std::string                definitionPoint() const;

  const Position            *position       () const;
  int                        positionPriority() const;

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
