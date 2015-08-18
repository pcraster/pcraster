#ifndef INCLUDED_CALC_BINDEDSYMBOL
#define INCLUDED_CALC_BINDEDSYMBOL

#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

namespace com {
  class Exception;
}
namespace pcrxml {
  class Data;
}

namespace calc {

//! user defined symbol
/*! This is a symbol with a name introduced by the user
 *  The position registered is the point of definition
 *  \todo
 *    definitions of d_externalBinding and d_externalName are
 *    not very clear, other names?
 */
class BindedSymbol : public Symbol {
private:
  //! value returned by externalName()
  std::string d_externalName;
  //! value returned by userName()
  std::string d_userName;
  //! the name used to look for externally by -r
  /*! this is the name as returned by externalName
   *  as if no -r is no used; this is ALWAYS a name exactly
   *  specified by the user.
   */
   Symbol d_externalBinding;

  void setBinded(const Symbol& bindedTo);
  void setBindingInUserName();

public:
  // CONSTRUCTORS
   BindedSymbol(const Symbol& parName);
   BindedSymbol(const Symbol& parName, const Symbol& bindedTo );
  // ACCESSORS
  const std::string& externalName() const;
  const std::string& userName() const;

  void  symError(const com::Exception& excep) const;

  void  posError(const std::string& msg) const;
  void  posError(const std::ostringstream& msg) const;

  void  setName(pcrxml::Data *d)const;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void  setInputFilePath();
};

}

#endif
