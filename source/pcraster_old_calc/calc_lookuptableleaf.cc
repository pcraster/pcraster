#include "stddefx.h"

#ifndef INCLUDED_CALC_LOOKUPTABLELEAF
#include "calc_lookuptableleaf.h"
#define INCLUDED_CALC_LOOKUPTABLELEAF
#endif

#ifndef INCLUDED_CALC_LOOKUPTABLEPARAMETER
#include "calc_lookuptableparameter.h"
#define INCLUDED_CALC_LOOKUPTABLEPARAMETER
#endif

#ifndef INCLUDED_CALC_INDEXSELECTED
#include "calc_indexselected.h"
#define INCLUDED_CALC_INDEXSELECTED
#endif

#ifndef INCLUDED_CALC_LOOKUPLINEAR
#include "calc_lookuplinear.h"
#define INCLUDED_CALC_LOOKUPLINEAR
#endif

#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_USEPAR
# include "calc_usepar.h"
#define INCLUDED_CALC_USEPAR
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_INDEXSELECTED
# include "calc_indexselected.h"
#define INCLUDED_CALC_INDEXSELECTED
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

/*!
  \file

  \todo com_exception.h inclusion is needed on gcc 2.96. Remove if we don't
        use that anymore.
*/

calc::LookupTableLeaf::LookupTableLeaf(
  const UsePar& pIn,
  VS vs,
  const std::vector<VS>& readKeys,
  bool linear):
  Symbol(pIn),d_index(pIn.createSelector())
{
  UsePar par(pIn);
  par.setInputFilePath();
  d_par = dynamic_cast<LookupTableParameter *>(script().findRightParameter(par,VS_TABLE));

  if (par.isArray()) {
   PRECOND(d_par);
   // init with index table, load now
   try {
     d_par->loadValuesFromIndexTable(vs,readKeys);
   }
   catch(const com::Exception& msg) {
     // pcrcalc/test287
     // pcrcalc/test288
     par.symError(msg);
   }
   return;
  }

  // else single value
  if (d_par == 0) { // load value
    std::vector <LookupTable *>tab(1);
    try {
      expectedFileType(par.externalName(),VS_TABLE);
      if (linear)
       tab[0] = new LookupLinear(vs);
      else
       tab[0] = new LookupTable(vs);
      tab[0]->setRecords(par.externalName(),readKeys);
    } catch (const com::Exception& msg) {
      // pcrcalc/test10[ab]
      // pcrcalc/test69
      par.symError(msg);
    }
    d_par = new LookupTableParameter(par,tab);
    script().addSymbol(d_par);
  }
}

calc::LookupTableLeaf::~LookupTableLeaf()
{
}

calc::LookupTable *calc::LookupTableLeaf::execute()
{
  return d_par->value(select());
}

size_t calc::LookupTableLeaf::select() const
{
  return d_index->select();
}

void calc::LookupTableLeaf::print(InfoScript& i)const
{
  i.parTag(name());
}

