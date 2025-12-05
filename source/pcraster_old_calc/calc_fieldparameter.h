#ifndef INCLUDED_OLDCALC_FIELDPARAMETER
#define INCLUDED_OLDCALC_FIELDPARAMETER

#include "calc_fieldtype.h"
#include "calc_subparameter.h"
#include "calc_field.h"

#include <vector>
#include <string>


namespace pcrxml {
  class Data;
}

namespace calc {

class FieldLeaf;
class UseDefNode;

//! paramater holding a Field
class  FieldParameter : public SubParameter {
 private:
  //! type of parameter
  FieldType d_type;
  //! first node of use/def chain
  const UseDefNode *d_chainBegin{};
  //! first node of use/def chain that is in dynamic
  const UseDefNode *d_firstChainNodeInDynamic{nullptr};
  //! last node of use/def chain
  UseDefNode *d_chainEnd{nullptr};
 protected:
  FieldParameter(const ParsPar& par, bool constant, bool input,VS vs, ST st);
 public:
  //! dtor
  ~FieldParameter() override {}
  // MANIPULATORS

  //! called if parameters scope is activated
  void goInScope() override =0;

  FieldType& restrictType();

  //! parameter value for stack
  /*! \throws Field::NotInitialized() if not initialized
   */
  virtual FieldHandle value(size_t index, bool lastUse)=0;

  //! check if field has a (single) vs, set delete point etc.
  void finalCheck() override;

  bool restrictUser(const FieldType& exprAssignedTo);

  // ACCESSORS
  const FieldType &fieldType()const;

  VS vs() const;
  VS symbolType() const override;

  const UseDefNode *firstDef() const;

  void addToChain(UseDefNode *l);

  void deleteValues();

  void setDataSubType(pcrxml::Data *d) const override;
  virtual double initialValue() const;

  void printSubSpecific(InfoScript& is)const override;

  //! restrictUser may return this as an exception
  class RestrictError {
    //! message
    std::string d_msg;
    public:
    //! ctor
    RestrictError(const std::string& msg):
        d_msg(msg) {}
    //! return message
    const std::string& what() const {
        return d_msg;
    }
  };

};

}

#endif
