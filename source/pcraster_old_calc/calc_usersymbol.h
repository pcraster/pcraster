#ifndef INCLUDED_CALC_USERSYMBOL
#define INCLUDED_CALC_USERSYMBOL

#ifndef INCLUDED_CALC_BINDEDSYMBOL
#include "calc_bindedsymbol.h"
#define INCLUDED_CALC_BINDEDSYMBOL
#endif

#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif

namespace pcrxml {
  class Data;
}

namespace calc {

class InfoScript;
class ParsPar;
class SymbolTable;

//! user defined symbol
/*! This is a symbol with a name introduced by the user.
 *  The position registered (base class Element) is the point of definition.
 */
class UserSymbol : public BindedSymbol {
   //! see symbolSequenceNr()
   int d_symbolSequenceNr;

   void setSymbolSequenceNr(int symbolSequenceNr);

   //! SymbolTable is allowed to call setSymbolSequenceNr()
   friend class SymbolTable;
 protected:
   UserSymbol(const BindedSymbol& parName);
 public:
  virtual ~UserSymbol();

  //! overwrite if actions are needed when symbol goes in scope
  virtual void goInScope();

  virtual UserSymbol *copyContents(const ParsPar& newName) const;

  //! the symbol type
  virtual        VS symbolType() const=0;

  //! overwrite if checking needs done
  /*! returns non-0 if symbol is copied and replaced
   */
  virtual void finalCheck();

  // ACCESSORS

  void print(InfoScript& i)const;
  virtual void printSpecific(InfoScript& i)const;

  virtual pcrxml::Data *createXmlData() const;

  int symbolSequenceNr() const;
};

}

#endif
