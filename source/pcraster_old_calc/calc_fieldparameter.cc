#include "stddefx.h"

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_CALC_FIELDPARAMETER
#include "calc_fieldparameter.h"
#define INCLUDED_CALC_FIELDPARAMETER
#endif

#ifndef INCLUDED_CALC_FIELDLEAF
#include "calc_fieldleaf.h"
#define INCLUDED_CALC_FIELDLEAF
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_FILEWRITER
#include "calc_filewriter.h"
#define INCLUDED_CALC_FILEWRITER
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_CALC_WRITEINFO
#include "calc_writeinfo.h"
#define INCLUDED_CALC_WRITEINFO
#endif

#ifndef INCLUDED_CALC_NEWXMLDATASUBTYPE
#include "calc_newxmldatasubtype.h"
#define INCLUDED_CALC_NEWXMLDATASUBTYPE
#endif


//! ctor
calc::FieldParameter::FieldParameter(
    const calc::ParsPar& par,
    bool constant, bool input, VS vs, ST st) :
  calc::SubParameter(par,constant,input),
  d_type(vs,st),
  d_firstChainNodeInDynamic(0),
  d_chainEnd(0)
{
}

//! delete the associated values
void calc::FieldParameter::deleteValues()
{
  for(size_t i=0; i < nrElements(); i++)
   try {
    FieldHandle v = value(i, true);
   } catch(const calc::Field::NotInitialized& v) {
     // not initialized skip, no need to delete
   }
}

//! vs of field
VS calc::FieldParameter::vs() const {
  return d_type.vs();
}

//! symbol type of field equals vs
VS calc::FieldParameter::symbolType() const {
  return vs();
}

//! return FieldType reference for restricting modification
calc::FieldType& calc::FieldParameter::restrictType()
{
  return d_type;
}

//! restrict possible types of field
/*  happens when reassigning a parameter more then once
 * \throws calc::FieldParameter::RestrictError if user made error
 */
bool calc::FieldParameter::restrictUser(
    const calc::FieldType& exprAssignedTo)
{
  std::string msg;
  try {
    return d_type.restrictUser(exprAssignedTo.vs(), exprAssignedTo.spatial());
  }
  // this happens if parameter is multiple times assigned pcrcalc/test2
  catch (const SyntaxVsClash& s) {
    msg = quote(userName())+" is defined as "+s.d_oldVs+" type on "
    +definitionPoint()+" and used here as "+s.d_newVs+" type";
  }
  catch (const SyntaxStClash& s) { // pcrcalc/test????
    msg = quote(userName())+" is defined as "+s.d_oldSt+" value on "
    +definitionPoint()+" and assigned here a "+s.d_newSt+" value";
  }
  throw RestrictError(msg);
  // never reached
}

//! return FieldType
const calc::FieldType& calc::FieldParameter::fieldType() const
{
  return d_type;
}

/*!
    \todo  IMPORTANT
           parameter that is first input and then output makes
           a mess of a number things (pcrcalc/test358): See
           if seperating in two pars, different lifespan/scope makes better code
           IDEA: First entry of symbol table (is input) can be replace by second
                 (new) entry when it is outputted
 */
void calc::FieldParameter::finalCheck()
{
  if (isOutput()) {

     // only worry on the correct type if written as output
     if (nrInSet(vs()) > 1) {
      // pcrcalc test38[a]
      posError("Use a conversion function to pick a data type for "+
      quote(userName())+"\npossible data type is "+toString(vs()));
     }

     std::string t("hacktest");
     if (isInput() && scriptConst().outputFilePath(t) != t ) {
      // is both input and output and -r is set
      // pcrcalc/test358
      posError("with use of -r: "+quote(userName())+" cannot be both input and output");
     }
   }
  // if it apears in dynamic, e.g. there is at least one node
  // of the chain in the dynamic, then the last node points
  // to that node, if d_firstChainNodeInDynamic may be 0 if
  // there is no use in the dynamic
  if (d_chainEnd) {
    d_chainEnd->addNextUseDef(d_firstChainNodeInDynamic);
  } else {
    //! TODO: WARNING
    //! otherwise never used!
  }
}

//! return first definition of parameter
const calc::UseDefNode *calc::FieldParameter::firstDef() const
{
  return d_chainBegin;
}

//! add \a l to use/def chain
void calc::FieldParameter::addToChain(calc::UseDefNode *l)
{
  if (d_chainEnd)
    d_chainEnd->addNextUseDef(l);
  else
    d_chainBegin = l;
  if (l->inDynamic() && (!d_firstChainNodeInDynamic))
    d_firstChainNodeInDynamic=l;
  d_chainEnd = l;
}

//! some hack to HTML out
/*!
   \todo
      do HTML out with XML/CSS and so on
 */
void calc::FieldParameter::printSubSpecific(calc::InfoScript& is)const
{
  fieldType().print(is);
}

//! hack to get value of constant binding
double calc::FieldParameter::initialValue() const
{
  return 0;
}

/*!
  \todo
    if a par like  A = 4; is never used it gets a multiple
    vs wich is hack here into UNKNOWN, warn never used?
 */
void calc::FieldParameter::setDataSubType(pcrxml::Data *d) const
{
   VS v=(nrInSet(vs())!=1) ? VS_UNKNOWN : vs();
   bool isSpatial=fieldType().spatial();

   if (reportedInDynamic()) {
     if (isSpatial)
       d->stack      = newDataSubType<pcrxml::Stack>(v);
     else
       d->timeSeries = newDataSubType<pcrxml::TimeSeries>(v);
   } else {
     if (isSpatial)
       d->map        = newDataSubType<pcrxml::Map>(v);
     else {
       d->nonSpatial = newDataSubType<pcrxml::NonSpatial>(v);
       if (isConstantBinding())
          d->nonSpatial->value = initialValue();
     }
   }
}
