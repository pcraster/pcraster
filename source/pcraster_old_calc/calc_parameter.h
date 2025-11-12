#ifndef INCLUDED_CALC_PARAMETER
#define INCLUDED_CALC_PARAMETER

#include "calc_usersymbol.h"



namespace calc {

//! parameter holding values
/*! the point where the parameter springs into
 *  existence, is the point stored in the Symbol
 *  base class
 */
class  Parameter : public UserSymbol {
 private:
  //! is parameter constant
  /*! If true then the value can not be assigned.
   *  Initialization of a constant occurs only in
   *  the binding.
   */
  const bool   d_constantBinding;
 protected:
  // CREATORS
  Parameter(const BindedSymbol& name, bool constant);

  ~Parameter() override;

  std::string  inputFilePath(const std::string& fileName) const;
  std::string outputFilePath(const std::string& fileName) const;

 public:

  // ACCESSORS
  bool         isConstantBinding()            const;
  void         printSpecific(InfoScript& i)   const override;

};


}

#endif
