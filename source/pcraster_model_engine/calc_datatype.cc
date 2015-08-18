#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif


/*!
  \file
  This file contains the implementation of the DataType class.
*/




//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATATYPE MEMBERS
//------------------------------------------------------------------------------

//! restrict a VS
/*!
 * \throw VSClash
 */
void calc::DataType::restrict(VS& vs, VS req)
{
  VS newVs = ::intersect(vs,req);
  if (newVs==VS_UNKNOWN)
    throw VSClash(vs,req);
  vs=newVs;
}

//------------------------------------------------------------------------------
// DEFINITION OF DATATYPE MEMBERS
//------------------------------------------------------------------------------

void calc::DataType::init()
{
  d_vs            =VS_ANYTHING;
  d_st            =ST_ALL;
  d_allowPromotion=false;
  d_resultType    =VS_FIELD;
  d_mapStackType  =MapStackType();
}

//! default ctor, any later restrictment will be valid
calc::DataType::DataType()
{
  init();
}

//! ctor for ASTNumber/NonSpatial by its value
calc::DataType::DataType(const double& value)
{
  init();
  d_vs=vsOfNumber(value);
  d_st=ST_NONSPATIAL;
}



//! ctor a Field datatype that may promote
/*! \param vs must be in VS_FIELD
    \param st must be ST_SPATIAL,ST_NONSPATIAL (= fixed), 
      ST_DERIVED (ST_EITHER)
 */
calc::DataType::DataType(VS vs, ST st)
{
  init();
  d_vs=vs;
  d_st=st;
}

//! ctor, d_st set on base of if vs is field or not
calc::DataType::DataType(VS vs)
{
  init();
  d_vs=vs;
  d_st= isIn(d_vs,VS_FIELD) ?ST_EITHER:ST_NON;
}

//! ctor a datatype that has fixed ST
calc::DataType::DataType(VS vs, bool spatial)
{
  init();
  d_vs=vs;
  d_st = spatial ? ST_SPATIAL:ST_NONSPATIAL;
}

calc::DataType::~DataType()
{
}

//! Copy constructor.
calc::DataType::DataType(DataType const& rhs):
   d_vs(rhs.d_vs),
   d_st(rhs.d_st),
   d_allowPromotion(rhs.d_allowPromotion),
   d_unit(rhs.d_unit),
   d_resultType(rhs.d_resultType),
   d_mapStackType(rhs.d_mapStackType),
   d_tableColTypes(rhs.d_tableColTypes)
{
}


//! Assignment operator.
calc::DataType& calc::DataType::operator=(DataType const& rhs)
{
  if (this != &rhs) {
    d_vs            = rhs.d_vs;
    d_st            = rhs.d_st;
    d_allowPromotion= rhs.d_allowPromotion;
    d_unit          = rhs.d_unit;
    d_resultType    = rhs.d_resultType;
    d_mapStackType  = rhs.d_mapStackType;
    d_tableColTypes = rhs.d_tableColTypes;
  }
  return *this;
}

//! set value of d_unit and vs to VS_S if !unit.none()
void calc::DataType::setUnit(const Dimension& unit)
{
  d_unit=unit;
  if (!d_unit.none())
    d_vs=VS_S;

}

//! get value of d_unit
const calc::Dimension& calc::DataType::unit() const
{
  return d_unit;
}




//! this can return a set > 2 !
VS calc::DataType::vs() const
{ return d_vs; }

//! is vs not a set but a single vs
bool calc::DataType::singleVs() const
{
 return(nrInSet(vs()) == 1);
}

bool calc::DataType::isField() const
{
 return isSubset(d_vs,VS_FIELD);
}

ST calc::DataType::st() const
{ return d_st; }

//! st is known and spatial
bool calc::DataType::stSpatial() const
{ return d_st==ST_SPATIAL; }
//! st is known and nonspatial
bool calc::DataType::stNonSpatial() const
{ return d_st==ST_NONSPATIAL; }
//! st is either
bool calc::DataType::stEither() const
{ return d_st==ST_EITHER; }


//! restrict the VS
/*!
 * \throw VSClash
 */
void calc::DataType::restrict(VS req)
{
  restrict(d_vs,req);
}

//! return if this allow \a  spatialState
ST calc::DataType::intersect(ST spatialState) const
{
  return (ST)((int)d_st & (int)spatialState);
}

//! restrict this on requirements
/*!
 * \param req  requirements for this that it must be 
 *             (VSClash::mustBeOneOf() etc.).
 *
 * \throws VSClash or STClash, in case of exception \a this
 *         is already (partially) modified
 */
void calc::DataType::restrict(
    const DataType& req)
{
  // common for all
  restrict(req.vs());

  switch(vs()) {
    case VS_TSS:
       restrict(d_resultType,req.resultType());
       break;
    case VS_MAPSTACK:
       restrict(d_resultType,req.resultType());
       d_mapStackType.update(req.mapStackType());
       break;
    case VS_TABLE:
      if (d_tableColTypes.empty())
        d_tableColTypes=req.tableColTypes();
      // empty: no info to restict;
      if (req.tableColTypes().empty())
        break;
      TableClash::checkNrOfColumns(
        d_tableColTypes.size(),req.tableColTypes().size());
      for (size_t i=0; i< d_tableColTypes.size();++i)
         try {
          restrict(d_tableColTypes[i],req.tableColTypes()[i]);
         } catch(const VSClash& c) {
          throw TableClash(i+1,c);
         }
      break;
    default: {
      // check spatial type pcrcalc60, pcrcalc258,pcrcalc346
      ST newSt = req.intersect(d_st);
      if (newSt == ST_ERROR) {
       // Only if req.allowPromotion() then \a this
       // (the DataType of an ASTPar)
       // may convert/promote from ST_NONSPATIAL to ST_SPATIAL.
       if (!req.allowPromotion())
         throw  STClash(stSpatial());
       // else convert implict to spatial
       newSt = ST_SPATIAL;
      }
      d_st=newSt;
    }
  }
}


/*!
 * \pre symbolType is in (VS_TSS (output),VS_MAPSTACK)
 * \pre resultType is in VS_FIELD set
 */
void calc::DataType::setResultType(VS symbolType,VS resultType)
{
  PRECOND(symbolType == VS_TSS || symbolType==VS_MAPSTACK);
  PRECOND(isSubset(resultType,VS_FIELD));
  d_vs        = symbolType;
  d_resultType= resultType;
}

//! reset tableColTypes, also change vs() to VS_TABLE
void calc::DataType::setTableColTypes(const std::vector<VS>& tableColTypes)
{
  d_vs           = VS_TABLE;
  d_tableColTypes= tableColTypes;
}

//! set value of d_allowPromotion
void calc::DataType::setAllowPromotion(bool allowPromotion)
{
  d_allowPromotion=allowPromotion;
}

//! get value of d_allowPromotion
bool calc::DataType::allowPromotion() const
{
  return d_allowPromotion;
}

void calc::DataType::promoteToSpatial()
{
  PRECOND(d_st==ST_EITHER || d_st!=ST_SPATIAL);
  d_st=ST_SPATIAL;
}

VS calc::DataType::resultType() const
{
  return d_resultType;
}

const std::vector<VS>& calc::DataType::tableColTypes() const
{
  return d_tableColTypes;
}

//! set value of d_mapStackType
void calc::DataType::setMapStackType(const MapStackType& mapStackType)
{
  d_mapStackType=mapStackType;
}

//! get value of d_mapStackType
const calc::MapStackType& calc::DataType::mapStackType() const
{
  return d_mapStackType;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


bool calc::operator==(
    const DataType& e1,
    const DataType& e2)
{
  if (e1.vs()!=e2.vs())
    return false;
  switch(e1.vs()) {
    case VS_TSS:
    case VS_MAPSTACK:
      return e1.resultType()==e2.resultType();
    case VS_TABLE:
      return e1.tableColTypes()==e2.tableColTypes();
    default:
      if (isIn(e1.vs(),VS_FIELD)) {
        // TODO d_unit in case of VS_S
        return e1.st()==e2.st();
      }
  }
  return true;
}

bool calc::operator!=(
    const DataType& e1,
    const DataType& e2)
{
  return !(e1==e2);
}


std::ostream & calc::operator<<(
    std::ostream& s,
    const calc::DataType& ft)
{
  s << "d_vs(" << ft.vs() << ")";
  const char *st("error");
  switch (ft.st()) {
    case ST_NONSPATIAL: st="nonspatial"; break;
    case ST_SPATIAL:    st="spatial";    break;
    case ST_EITHER:     st="either";     break;
    case ST_ERROR:      st="error";    break;
    case ST_NON:         st="non";     break;
    case ST_ALL:         st="all";     break;
  }
  s << "d_st(" << st << ")";
  s << "d_resultType(" << ft.resultType() << ")";
  for(size_t i=0; i < ft.tableColTypes().size(); ++i) // empty if not a table
    s << "\n\t d_tableColTypes(" << i << ":" << ft.tableColTypes()[i] << ")";
  return s;
}
