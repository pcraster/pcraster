#ifndef INCLUDED_GEO_FILECREATETESTER
#define INCLUDED_GEO_FILECREATETESTER

#include "stddefx.h"
#include "com_math.h"
#include "com_pathname.h"


namespace geo {

class CSFMap;


//! Tests the creation of a data file by comparing it against another file
/*!
   Currently it only supports the existence and certain features of a CSF map.
   or TimeSeries with a header.
   Envisioned is a more generic class support all kind of file format features,
   and or comparing the data contents of different grid formats.
 */
class FileCreateTester
{

private:

  //  Assignment operator. , default
  //  FileCreateTester&           operator=           (const FileCreateTester&);

  //  Copy constructor, default
  //               FileCreateTester               (const FileCreateTester&);

  //! if existant, this file is removed in the ctor
  com::PathName   d_fileToCreate;
  //! holds difference map name, empty if no diff map needed
  com::PathName   d_diffMapName;

  bool             d_percentageDifference{false};
  double           d_csfCellEpsilon{COM_DEFAULT_EPSILON};

  bool equalToCsf(const com::PathName& pn, bool throwWhatDifferent) const;
  bool equalToTss(const com::PathName& pn, bool throwWhatDifferent) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FileCreateTester               (const com::PathName& fileToCreate,
                                                   bool removeNow=true);

  /* virtual */    ~FileCreateTester              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------


  void             setCsfCellEpsilon          (double csfCellEpsilon);
  void             setDifferenceFile          (const com::PathName& diffMapName);
  void             setPercentageDifference    (bool percentageDifference);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool   equalTo  (const com::PathName& eqTo, bool throwWhatDifferent=true) const;
  bool             percentageDifference() const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
