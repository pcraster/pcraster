#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_XMLDATATYPE
#include "calc_xmldatatype.h"
#define INCLUDED_CALC_XMLDATATYPE
#endif

// Library headers.
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif



/*!
  \file
  This file contains the implementation of the XMLDataType class.
*/



namespace calc {

//------------------------------------------------------------------------------

/*
class XMLDataTypePrivate
{
public:

  XMLDataTypePrivate()
  {
  }

  ~XMLDataTypePrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC XMLDATATYPE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF XMLDATATYPE MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

static VS  xml2VS(pcrxml::DataType const& d) {
   VS vs=VS_UNKNOWN;
   if (d.boolean().present())
    vs=unionSet(vs,VS_B);
   if (d.nominal().present())
    vs=unionSet(vs,VS_N);
   if (d.ordinal().present())
    vs=unionSet(vs,VS_O);
   if (d.scalar().present())
    vs=unionSet(vs,VS_S);
   if (d.directional().present())
    vs=unionSet(vs,VS_D);
   if (d.ldd().present())
    vs=unionSet(vs,VS_L);

   return vs;
}

static ST xml2ST(pcrxml::SpatialType const& s) {
   switch (s) {
     case pcrxml::SpatialType::Spatial: return ST_SPATIAL;
     case pcrxml::SpatialType::NonSpatial: return ST_NONSPATIAL;
     case pcrxml::SpatialType::Either: return ST_EITHER;
   }
   return ST_NON;
}

DataType xml2DataType(
    pcrxml::FieldValueOrType const& f) {

    VS vs=VS_ANYTHING;
    ST st=ST_ALL;
    if (f.dataType().present())
      vs=xml2VS(f.dataType().get());
    if (f.spatialType().present())
      st=xml2ST(f.spatialType().get());

    return DataType(vs,st);
}

DataType xml2DataType(
    pcrxml::FieldType const& f) {
    return DataType(
     xml2VS(f.dataType()),
     xml2ST(f.spatialType()));
}


template<typename T>
 static T toXML(DataType const& dt)
{
    T x;

    if (isIn(VS_B,dt.vs()))
      x.boolean(pcrxml::Boolean());
    if (isIn(VS_N,dt.vs()))
      x.nominal(pcrxml::Nominal());
    if (isIn(VS_O,dt.vs()))
      x.ordinal(pcrxml::Ordinal());
    if (isIn(VS_S,dt.vs())) {
      x.scalar(pcrxml::Scalar());
      Dimension const& d(dt.unit());
#     define SET_DIM(Enum,Elem)                         \
      if (d[Dimension::Enum] != 0)                      \
        x.scalar()->Elem().set(d[Dimension::Enum]);
      SET_DIM(Length,length);
      SET_DIM(Mass,mass);
      SET_DIM(Time,time);
      SET_DIM(ElectricCurrent,electricCurrent);
      SET_DIM(Temperature,temperature);
      SET_DIM(AmountOfSubstance,amountOfSubstance);
      SET_DIM(LuminousIntensity,luminousIntensity);
      SET_DIM(Currency,currency);
#     undef SET_DIM
    }
    if (isIn(VS_D,dt.vs()))
      x.directional(pcrxml::Directional());
    if (isIn(VS_L,dt.vs()))
      x.ldd(pcrxml::Ldd());

    return x;
}

static pcrxml::DataTypeOfValue toXMLDataTypeOfValue(DataType const& dt)
{
  return toXML<pcrxml::DataTypeOfValue>(dt);
}

pcrxml::DataType toXMLDataType(DataType const& dt)
{
  return toXML<pcrxml::DataType>(dt);
}

pcrxml::FieldTypeOfValue toXMLFieldTypeOfValue(DataType const& dt)
{
 pcrxml::DataTypeOfValue pdt(toXMLDataTypeOfValue(dt));
 pcrxml::SpatialType pst(st2XML(dt.st()));
 return pcrxml::FieldTypeOfValue(pdt,pst);
}

pcrxml::FieldType  toXMLFieldType(DataType const& dt)
{
 pcrxml::DataType pdt(toXMLDataType(dt));
 pcrxml::SpatialType pst(st2XML(dt.st()));
 return pcrxml::FieldType(pdt,pst);
}

pcrxml::SpatialType st2XML(ST st)
{
    switch(st) {
      case ST_SPATIAL:    return pcrxml::SpatialType::Spatial;
      case ST_NONSPATIAL: return pcrxml::SpatialType::NonSpatial;
      case ST_EITHER:     return  pcrxml::SpatialType::Either;
      default:
                          DEVELOP_PRECOND(false);
                          return  pcrxml::SpatialType::Either;
    };
}

} // namespace calc

