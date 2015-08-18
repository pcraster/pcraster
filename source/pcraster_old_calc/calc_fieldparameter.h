#ifndef INCLUDED_CALC_FIELDPARAMETER
#define INCLUDED_CALC_FIELDPARAMETER

#ifndef INCLUDED_CALC_FIELDTYPE
#include "calc_fieldtype.h"
#define INCLUDED_CALC_FIELDTYPE
#endif

#ifndef INCLUDED_CALC_SUBPARAMETER
#include "calc_subparameter.h"
#define INCLUDED_CALC_SUBPARAMETER
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

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
  const UseDefNode *d_chainBegin;
  //! first node of use/def chain that is in dynamic
  const UseDefNode *d_firstChainNodeInDynamic;
  //! last node of use/def chain
  UseDefNode *d_chainEnd;
 protected:
  FieldParameter(const ParsPar& par, bool constant, bool input,VS vs, ST st);
 public:
  //! dtor
  virtual ~FieldParameter() {};
  // MANIPULATORS

  //! called if parameters scope is activated
  virtual void goInScope()=0;

  FieldType& restrictType();

  //! parameter value for stack
  /*! \throws Field::NotInitialized() if not initialized
   */
  virtual FieldHandle value(size_t index, bool lastUse)=0;

  //! check if field has a (single) vs, set delete point etc.
  void finalCheck();

  bool restrictUser(const FieldType& exprAssignedTo);

  // ACCESSORS
  const FieldType &fieldType()const;

  VS vs() const;
  VS symbolType() const;

  const UseDefNode *firstDef() const;

  void addToChain(UseDefNode *l);

  void deleteValues();

  void setDataSubType(pcrxml::Data *d) const;
  virtual double initialValue() const;

  void printSubSpecific(InfoScript& is)const;

  //! restrictUser may return this as an exception
  class RestrictError {
    //! message
    std::string d_msg;
    public:
    //! ctor
    RestrictError(const std::string& msg):
        d_msg(msg) {};
    //! return message
    const std::string& what() const {
        return d_msg;
    }
  };

};

}

#endif
