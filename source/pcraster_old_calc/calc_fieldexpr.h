#ifndef INCLUDED_CALC_FIELDEXPR
#define INCLUDED_CALC_FIELDEXPR

#ifndef INCLUDED_CALC_ELEMENT
#include "calc_element.h"
#define INCLUDED_CALC_ELEMENT
#endif

#ifndef INCLUDED_CALC_VS
# include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

namespace calc {

class FieldType;
class FieldStack;
class InfoScript;
class Compressor;

//! expression returning a field
class FieldExpr : public Element {
protected:
  FieldExpr(const Element& pos);

  virtual const FieldType &fieldType()const=0;
  friend class FieldLeft; // for fieldType only
public:
  virtual ~FieldExpr();
  // MANIPULATORS

  //! buids its own types and call for sub-expression
  virtual void buildTypesRecursive(VS resultVsSet)=0;

  //! returns FieldType for further type restrictment
  virtual FieldType& restrictType()=0;

  //! build up use-def chain
  virtual void prepareExecution()=0;
  //! execution
  virtual void execute(FieldStack& stack)=0;
  //! branch is not executed (in if-exp)
  /*! need to go through branch to
   *  delete last use nodes
   */
  virtual void skipExecution()=0;

  // ACCESSORS

  VS  vs() const;
  bool spatial() const;

  //! is this a Constant ?
  virtual bool isConstant() const;

  virtual bool isFieldLeaf() const;
  bool isEndNode() const;
  virtual void print(InfoScript &si) const=0;
  FieldHandle createResultField()const;

  const Compressor& compressor() const;
};

}

#endif
