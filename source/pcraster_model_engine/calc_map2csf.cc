#include "stddefx.h"

#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif

#ifndef INCLUDED_MISC
#include "misc.h" // BITSET macros
#define INCLUDED_MISC
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

/*! pick one value scale from \a vsSet that is the
 * biggest from a set of value scales assuming:
 * VS_SD &gt; VS_NO &gt; VS_BL and
 * CR_REAL4 &gt; CR_INT4 &gt; CR_UINT1
 */
CSF_CR calc::biggestCellRepr(
  VS vsSet) /* set of value scales */
{
  if(isIn(VS_SD, vsSet))
    return CR_REAL4;
  if(isIn(VS_NO, vsSet))
    return CR_INT4;
  if(isIn(VS_BL, vsSet))
    return CR_UINT1;
  POSTCOND(FALSE); // NEVER
  return CR_REAL4;
}

/*!
 * \param vs can be a set: biggestCellRepr called
 */
size_t calc::bytesPerCell(VS vs) {
  if (biggestCellRepr(vs) == CR_UINT1)
    return 1;
  return 4;
}

//! maps a calc-internal vs type to a csf vs types
/*! \param vs must hold a single vs, not a set
 */
PCR_DLL_FUNC(CSF_VS) calc::vs2CsfVs(VS vs)
{
  const CSF_VS csfVs[] = { VS_BOOLEAN,VS_NOMINAL, VS_ORDINAL,
                         VS_SCALAR, VS_DIRECTION, VS_LDD};
  PRECOND(NRBITSET_TYPE(vs, VS) == 1);
  PRECOND(FIRSTBITSET_TYPE(vs, VS) < (int)ARRAY_SIZE(csfVs));
  PRECOND(VS_B==1);       /* boolean */
  PRECOND(VS_N==2);       /* nominal */
  PRECOND(VS_O==4);       /* ordinal */
  PRECOND(VS_S==8);       /* scalar */
  PRECOND(VS_D==16);      /* direction */
  PRECOND(VS_L==32);      /* ldd */

  return csfVs[FIRSTBITSET_TYPE(vs, VS)];
}

//! maps a csf vs to a pcrcalc-internal vs type
/*! \throws com::Exception if \a vs is VS_NOTDETERMINED
 */
PCR_DLL_FUNC(VS) calc::csfVs2vs(
  CSF_VS vs)
{
  switch(vs)
  {
    case VS_CLASSIFIED    : return VS_CLAS;
    case VS_CONTINUOUS    : return VS_CONT;
    case VS_BOOLEAN       : return VS_B;
    case VS_NOMINAL       : return VS_N;
    case VS_ORDINAL       : return VS_O;
    case VS_SCALAR        : return VS_S;
    case VS_DIRECTION     : return VS_D;
    case VS_LDD           : return VS_L;
    case VS_NOTDETERMINED :
        throw com::Exception("map does not have a value scale");
        break;
    default:
      PRECOND(FALSE); // NEVER
  }
  /* never reached */
  return VS_FIELD;
}
