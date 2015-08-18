#ifndef INCLUDED_CALC_SYMBOL
#define INCLUDED_CALC_SYMBOL

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ELEMENT
#include "calc_element.h"
#define INCLUDED_CALC_ELEMENT
#endif

namespace calc {

class LexToken;
class ExtSym;

//! named element of a script
/*! single id or operator has a name
 */
class Symbol : public Element {
private:
  //! name of the symbol, like id,+,etc.., never empty except if default ctor is used
  std::string d_name;

public:
  /*! we wished to make default ctor forbidden but that makes
   *  the ANLTR grammar very difficult, now we empty() in the parser
   */
  Symbol();
  Symbol(const Element& e, const std::string& name);
  Symbol(IScript *script, const ExtSym& s);
  Symbol(IScript *script, const std::string& name, const Position *pos);
  Symbol(IScript *script, const LexToken*    token);
  virtual ~Symbol();
  void setName(const std::string& newName);
  // ACCESSORS
  virtual const std::string& name() const;
  //! return name quoted
  std::string qName() const;

  bool empty() const;

  bool   isNumber() const;
  double toNumber() const;
};

bool operator<(const Symbol& lhs, const Symbol& rhs);
bool operator==(const Symbol& lhs, const Symbol& rhs);

}

#endif
