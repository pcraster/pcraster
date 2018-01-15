#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.
#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
#ifndef INCLUDED_GEO_UTIL
#include "geo_util.h"
#define INCLUDED_GEO_UTIL
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_TABLE
#include "com_table.h"
#define INCLUDED_COM_TABLE
#endif

// Module headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_GEO_CSFMAP
#include "geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif

#ifndef INCLUDED_GEO_CSFRASTER
#include "geo_csfraster.h"
#define INCLUDED_GEO_CSFRASTER
#endif

/*!
  \file
  This file contains the implementation of the FileCreateTester class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FILECREATETESTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FILECREATETESTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*! 
 *  \param fileToCreate if existant, this file is removed here if \a removeNow is true
 *  \param removeNow    remove
 */
geo::FileCreateTester::FileCreateTester(const com::PathName& fileToCreate,
    bool removeNow):
  d_fileToCreate(fileToCreate),
  d_percentageDifference(false),
  d_csfCellEpsilon(COM_DEFAULT_EPSILON)
{
  if (removeNow && com::exists(fileToCreate))
     com::remove(fileToCreate);
}

//! dtor
geo::FileCreateTester::~FileCreateTester()
{
}

namespace geo {
  class ThrowOrReturn {
    com::PathName d_m1, d_m2;
    bool  d_throwWhatDifferent;
  public:
    ThrowOrReturn(const com::PathName& m1, const com::PathName& m2,
                  bool throwWhatDifferent):
      d_m1(m1),d_m2(m2),
      d_throwWhatDifferent(throwWhatDifferent)
    {}
    bool falseOrThrow(const std::string& inMsg) {
      if(d_throwWhatDifferent) {
        std::ostringstream msg;
        msg << "Not equal to " << d_m2 << ": " << inMsg;
        throw com::FileError(d_m1,msg.str());
      }
      return false;
    }
 };

class DiffMap : boost::noncopyable
{
    //! 0 if no difference
    Raster<UINT1> *d_values;
    bool d_diffMapWanted;
    bool d_diffNoted;
    ThrowOrReturn& d_tr;
    RasterSpace    d_rs;
    void init() {
      if (!d_values) {
        d_values = new Raster<UINT1>(d_rs);
        for(size_t i=0; i < d_values->nrCells(); i++)
          (*d_values)[i] = MV_UINT1;
      }
    }
   public:
    DiffMap(bool diffMapWanted, ThrowOrReturn& tr,const RasterSpace& rs)
      : d_values(0), d_diffMapWanted(diffMapWanted),
        d_diffNoted(false), d_tr(tr) ,d_rs(rs)
    {}
    ~DiffMap() {
       delete d_values;
    }
    void setMVDiffer(size_t index) {
      d_diffNoted=true;
      if (d_diffMapWanted) {
         init();
         (*d_values)[index] = 1;
      } else
         d_tr.falseOrThrow("MV cells differ");
    }
    void setValuesDiffer(size_t index) {
      d_diffNoted=true;
      if (d_diffMapWanted) {
         init();
         (*d_values)[index] = 2;
      } else
         d_tr.falseOrThrow("value cells differ");
    }
    //! return 0 if no diffs detected or wanted
    UINT1 *diffs()
    {
      if (d_values)
       return d_values->cells();
      return 0;
    }
    bool diffNoted() const { return d_diffNoted; }
 };
}

//! compare if \a d_fileToCreate and \a eqTo are equal in file format
/*! Current only support of CSFMap files:
 *  Tests on all aspects such as location attributes, data types and values
 *  The values are compared using com::equal_epsilon() to correct for float
 *  roundings
 *  \param eqTo
 *  \param throwWhatDifferent if true (default) compareTo never returns on false but
 *                            throws a com::FileException specifying what does differ.
 *  \todo Rcompare call here is actuall a == on RasterSpace objects
 */
bool geo::FileCreateTester::equalTo(const com::PathName& eqTo,
                                    bool throwWhatDifferent) const
{
   if (!com::exists(d_fileToCreate)) {
     if (throwWhatDifferent)
       throw com::FileError(d_fileToCreate,"is not created");
     return false;
   }

   if (isCSFStack(d_fileToCreate)) {

     return equalToCsf(eqTo,throwWhatDifferent);
   } else {
    // isTimeSeriesFile(const com::PathName& pathName);
     return equalToTss(eqTo,throwWhatDifferent);
   };
}

bool geo::FileCreateTester::equalToTss(
   const com::PathName& eqTo,
   bool throwWhatDifferent) const
{
  com::Table tss1;
  std::ifstream i1;
  com::open(i1,d_fileToCreate);
  i1 >> tss1;

  com::Table tss2;
  std::ifstream i2;
  com::open(i2,eqTo);
  i2 >> tss2;

  ThrowOrReturn tr(d_fileToCreate,eqTo,throwWhatDifferent);

  if (tss1.nrRecs() != tss2.nrRecs())
    return tr.falseOrThrow(" nr of timesteps differ");
  if (tss1.nrCols() != tss2.nrCols())
    return tr.falseOrThrow(" nr of columns differ");

  std::string firstErrMsg;

  for(size_t r=0; r < tss1.nrRecs(); ++r)
   for(size_t c=0; c < tss1.nrCols(); ++c) {
     double v1=tss1.value(c,r);
     double v2=tss2.value(c,r);
     double diff=v1-v2;
     if (com::equal_epsilon(v1,v2,0.00001))
       diff=0; // do not write epsilon diff to difference file
     else
      if (firstErrMsg.empty()) {
        std::ostringstream str;
        str << "value at (timestep,colNr) (" 
            << r+1 << "," << c+1 << ") differ:\n  "
            << v1  << "-" << v2 << "=" << diff << std::endl;
        firstErrMsg=str.str();
     }
     if (c) { // skip time column
       if (diff !=0 && d_percentageDifference)
        tss1.setValue(c,r,com::fractionDifference(v1,v2));
       else
        tss1.setValue(c,r,diff);
     }
   }

  if (!firstErrMsg.empty()) {
   if(!d_diffMapName.isEmpty()) {
    std::ofstream diff;
    com::open(diff,d_diffMapName);
    diff << tss1;
   }
   return tr.falseOrThrow(firstErrMsg);
  }

  return true;
}

bool geo::FileCreateTester::equalToCsf(
   const com::PathName& eqTo,
   bool throwWhatDifferent) const
{
     CSFMap map1(d_fileToCreate);
     CSFMap map2(eqTo);
   ThrowOrReturn tr(map1.filename(),map2.filename(),throwWhatDifferent);

  if(map1.rasterSpace() != map2.rasterSpace())
    return tr.falseOrThrow("location attributes differ");
  if( map1.valueScale() != map2.valueScale() )
    return tr.falseOrThrow("data type (aka value scale) differ");

  // side effect useAs needed for min/max checking?
  CSFRaster<REAL8> r1(map1);
  CSFRaster<REAL8> r2(map2);


  DiffMap dm(!d_diffMapName.isEmpty(),tr,map1.rasterSpace());

  for(size_t i=0;i < r1.nrCells();i++)
   if (pcr::isMV(r1[i])) {
    if (!pcr::isMV(r2[i]))
     dm.setMVDiffer(i);
   } else {
    if (pcr::isMV(r2[i]))
     dm.setMVDiffer(i);
    else
     if (!com::equal_epsilon(r1[i],r2[i], d_csfCellEpsilon))
      dm.setValuesDiffer(i);
   }

  if (dm.diffs()) {
    POSTCOND(!d_diffMapName.isEmpty());
    CSFMap diffMap(d_diffMapName.toString(),map1,VS_NOMINAL,CR_UINT1);
    diffMap.putCells(dm.diffs());
  }

  double m1,m2;
  if (map1.min(&m1) != map2.min(&m2) || !com::equal_epsilon(m1,m2))
    return tr.falseOrThrow("header minimum differ");
  if (map1.max(&m1) != map2.max(&m2) || !com::equal_epsilon(m1,m2))
    return tr.falseOrThrow("header maximum differ");

  return !dm.diffNoted();
}

//! set to write file on compareTo()
/*! if set than a subsequent call to compareTo() will write a difference file if
    compareTo result
 */
void geo::FileCreateTester::setDifferenceFile(const com::PathName& diffMapName)
{
  d_diffMapName = diffMapName;
}

void geo::FileCreateTester::setCsfCellEpsilon(double csfCellEpsilon)
{
  d_csfCellEpsilon=csfCellEpsilon;
}

//! set value of d_percentageDifference
void geo::FileCreateTester::setPercentageDifference(bool percentageDifference)
{
  d_percentageDifference=percentageDifference;
}

//! get value of d_percentageDifference
bool geo::FileCreateTester::percentageDifference() const
{
  return d_percentageDifference;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
