#ifndef INCLUDED_CALC_RUNTIMEENV
#define INCLUDED_CALC_RUNTIMEENV



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

// Module headers.
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif
#ifndef INCLUDED_CALC_RUNTIMESTACK
#include "calc_runtimestack.h"
#define INCLUDED_CALC_RUNTIMESTACK
#endif
#ifndef INCLUDED_CALC_DATATABLE
#include "calc_datatable.h"
#define INCLUDED_CALC_DATATABLE
#endif
#ifndef INCLUDED_CALC_TIMER
#include "calc_timer.h"
#define INCLUDED_CALC_TIMER
#endif



namespace geo {
  class RasterSpace;
}
namespace com {
  class TempDirectory;
}
namespace pcrxml {
  class RunContext;
}



namespace calc {


class ICellIterator;
class SpatialPacking;
class IFieldRDConversion;
class IOStrategy;
class RunTimeEnvSettings;
class Field;
class FieldWriter;
class ICachedObject;
class MemoryExchangeItem;

//! run time for execution of operations
/*!
 * The run time environment is fixed for a specific RasterSpace configuration
 */
class RunTimeEnv
{
private:

  //! Assignment operator. NOT IMPLEMENTED.
  RunTimeEnv&           operator=           (const RunTimeEnv& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   RunTimeEnv               (const RunTimeEnv& rhs);

  //! Default Ctor. NOT IMPLEMENTED.
                   RunTimeEnv               ();

  void             init                     (const RunTimeEnvSettings& s);
  void             checkConstraints         (const RunTimeEnvSettings& s) const;
  void             clean                    ();

  bool                        d_enableCache;

  IOStrategy*                 d_ioStrategy;
  com::TempDirectory*         d_swapDir;

  /*!
   * all Field data on this stack is owned by d_stack itself and must
   * be cleaned, other DataValue's are not owned.
   */
  RunTimeStack                d_stack;

  DataTable                   d_data;

  typedef std::map<std::string, FieldWriter *> Writers;
  Writers                     d_writers;

  //! map Field::src() to ICachedObject
  typedef std::map<const void*, const ICachedObject *> Cache;
  Cache                       d_cache;

  Timer            d_timer;
  //! FTTB only tss output -1 option, other sync stuff not
  bool                        d_syncEachTimeStep;

  //! print some profiling information
  bool                        d_profile;

  //!  0 if none active, not owned
  ICellIterator*              d_cellIterator;

  //  //! current active array selections selections
  //  std::map<class IndexFoo*, class IndexBar*>    d_indexCurrent;

  void                debugMVAssignments (const Field* f) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    RunTimeEnv              (const RunTimeEnvSettings&  fs);
                    RunTimeEnv              (const geo::RasterSpace&  rs);

  /* virtual */    ~RunTimeEnv              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setSyncEachTimeStep (bool   syncEachTimeStep);

  void             load                (const ASTSymbolInfo& i);

  void             start               ();
  void             finish              ();

  void             setTimer            (const Timer& timer);
  void             incCurrentTimeStep  ();

  void             pushDataValue       (const DataValue     *d);
  void             pushField           (const Field *f);
  void             pushValue           (const ASTPar* p);

  DataValue*       popDataValue        ();
  Field*           popField            ();

  DataValue*       load                (std::string const&   name,
                                        std::string const&   externalName,
                                        bool                 lastUse);
  void             setMemoryExchangeData(void **data);

  bool             stackedCondition    ();

  void             assignStackTop      (const std::vector<ASTPar *> pars);
  void             assignStackTop      (const ASTPar *p);
  void             deleteValue         (const std::string& parName);
  void             deleteAllValues     ();

  void             assignOutTss        (const std::string& tss);

  void             transferIfCached    (const void*    fieldSrcValue,
                                        const ICachedObject *obj);

  void  transferMemoryExchangeItemIntoDataTransferArray(
                                        MemoryExchangeItem* item);

  const ICachedObject*    cachedObject        (const void*   fieldSrcValue);
  void                    deleteCacheEntry    (const void*   fieldSrcValue);
  void                    setCellIterator     (ICellIterator* cellIterator);
  IOStrategy&             ioStrategy          ();

  void                    cleanOnException    ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool                    syncEachTimeStep    () const;

  ICellIterator*          cellIterator        () const;

  Field*                  createResultField   (const DataType& d) const;

  const geo::RasterSpace& rasterSpace         () const;
  const SpatialPacking&   spatialPacking      () const;
  const IFieldRDConversion& ifieldRDConversion() const;
  const Timer&            timer               () const;

  const IOStrategy&       ioStrategy          () const;

  size_t                  stackSize           () const;

  bool                    empty               () const;
  const DataTable&        dataTable           () const;
  const com::PathName&    outputDirectory     () const;

  pcrxml::RunContext*     createXMLContext    () const;
};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
