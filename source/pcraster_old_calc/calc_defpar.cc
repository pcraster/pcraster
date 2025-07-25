#include "stddefx.h"
#include "calc_defpar.h"
#include "calc_arraydefinition.h"
#include "calc_statementblock.h" // findSymbol only

//! indices are array-definition's
calc::DefPar::DefPar(const ConstructPar& p):
  ParsPar(p)
{
  std::vector<const ArrayDefinition* > v(d_index.size());

  for (size_t i = 0; i < d_index.size(); i++)
    v[i] = dynamic_cast<const ArrayDefinition *>
            (d_block->findSymbol(&d_index[i],VS_ARRAY,true));
  d_descriptor = ArrayDefVector(v);
}

//! return the descriptor
const calc::ArrayDefVector& calc::DefPar::descriptor() const
{
 return d_descriptor;
}

#include "calc_arraydefvector.h"
#include "calc_fieldmapinputparameter.h"
#include "calc_fieldnrparameter.h"
#include "calc_lookuptableparameter.h"
#include "calc_fieldtype.h"
#include "calc_iscript.h"
#include "com_exception.h"
#include "calc_statementblock.h"
#include "calc_indextable.h"

void calc::DefPar::initError(
      const com::Exception& msg) const
{
   throw com::Exception("While initializing "+qName()+":\n"+msg.messages());
}


//! create a parameter by reading an indexTable
/*!
    Create an indexed
        calc::FieldMapInputParameter,
        calc::FieldNrParameter or
     calc::LookupTableParameter parameter object
    whose contents is defined in \a it

    the index table itself is also  added to the symbol table

    \throws
       com::Exception if error occured
    \return
      the created parameter
 */
calc::SubParameter *calc::DefPar::indexTable(
  const IndexTable *it,
  bool constant,
  const FieldType& useType) const
{
  if (!(it->arrayDefVector()==descriptor())) {
     // pcrcalc/test277
     std::ostringstream msg;
     msg << it->qName()
         << " is defined as an index-table on "
         << it->arrayDefName()
         << " type on "
         << it->definitionPoint()
         << " and used here as "
         << descriptor().name();
     throw com::Exception(msg.str());
  }
  SubParameter *v = nullptr;
  if (isIn(useType.vs(),VS_FIELD)) {
     if (useType.spatial()) {
       std::vector<std::string>vals;
       VS mapVs;
       try {
            mapVs = it->fieldMapValues(*this,vals);
       } catch (com::Exception& msg) {
            initError(msg);
       }
       v = new FieldMapInputParameter(*this,constant,mapVs,vals,
                                       it->scriptConst().ioFieldStrategy());
     } else {
       std::vector<double>vals;
       try {
            it->fieldNrValues(*this, useType.vs(),vals);
       } catch (com::Exception& msg) {
            initError(msg);
       }
       v = new FieldNrParameter(*this,constant,vals,useType.vs());
     }
 } else {
    PRECOND(useType.vs()== VS_TABLE);
    try {
     it->tableNameVerify(*this);
    } catch (com::Exception& msg) {
     initError(msg);
    }
    v = new LookupTableParameter(*this,it);
  }
  return v;
}
