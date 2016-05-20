#ifndef INCLUDED_CALC_IOSTRATEGY
#define INCLUDED_CALC_IOSTRATEGY



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
// Module headers.
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif
#ifndef INCLUDED_CALC_TIMER
#include "calc_timer.h"
#define INCLUDED_CALC_TIMER
#endif
#ifndef INCLUDED_CALC_RUNDIRECTORY
#include "calc_rundirectory.h"
#define INCLUDED_CALC_RUNDIRECTORY
#endif


namespace calc {
  // IOStrategy declarations.
  class StackInfo;
  class AreaMap;
  class ASTSymbolTable;
  class RunDirectory;
  class SpatialPacking;
  class Field;
  class Timer;
  class StackInput;
  class ASTSymbolInfo;
  class FieldWriter;
  struct GridStat;
  class DataType;
  class MapStackType;
  class IoFieldStrategy;
  class MemoryExchangeItem;
}
namespace geo {
  class RasterSpace;
}
namespace pcrxml {
  class AreaMapScript;
  class ComputationMask;
}


namespace calc {



//! Handle IO for an execution
class IOStrategy
{

private:

  //! never 0
  IoFieldStrategy* d_fs;

  //! never 0
  AreaMap*         d_areaMap;

  SpatialPacking* d_spatialPacking;

  RunDirectory    d_runDirectory;
  Timer           d_timer;

  typedef std::map<std::string, boost::shared_ptr<MemoryExchangeItem> > MemoryData;
  MemoryData        d_memoryData;
  //! reference to user passed DataTransferArray
  /*!
     elements can be updated
   */
  void**            d_dataTransferArray;
  //! registers what elements of d_dataTransferArray are passed as 0 by user
  std::vector<bool> d_dataTransferArrayUser0;

  /*! \brief the name map of the map that is written
   *         if debugMVAssignments find MV assignments
   * non-empty denotes -d on
   */
  std::string     d_debugMVAssignmentsMap;
  bool            d_mvCompression;
  bool            d_writeEachTimeStep;


  //! Assignment operator. NOT IMPLEMENTED.
  IOStrategy&           operator=           (IOStrategy const& rhs);

  MemoryExchangeItem*    memoryValue        (std::string const& name) const;
//  bool     hasMemoryValue                 (std::string const& name) const;
  void     setMemoryValue                   (const std::string& name,
                                             const Field *f) const;
  void     initResolve                      ();
  DataType resolveInputField                (const std::string& newExternalName,
                                             const DataType& dt);

  void             readField                (void *dest,
                                             const std::string& name,
                                             const DataType& type) const;

  GridStat         writeFieldUnpacked       (const std::string& fileName,
                                             const Field *f);
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  //! Copy constructor.
                   IOStrategy               (IOStrategy const& rhs);

                   IOStrategy               ();

  virtual          ~IOStrategy              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setRunDirectory          (const com::PathName& runDirectoryPath);
  void             setWriteEachTimeStep     (bool writeEachTimeStep);
  void             setDebugMVAssignmentsMap (const std::string& debugMVAssignmentsMap);
  void             setMVCompression         (bool mvCompression);


  void             setRasterSpace           (const geo::RasterSpace& rs);

  void             resolve                  (ASTSymbolTable& symbols,
                                             std::string const& areaMap,
                                             const Timer& timer);
//void             configureSymbols         (ASTSymbolTable& symbols,
//                                           const XMLReflection& xr);
  void             setXMLAreaMapScript      (pcrxml::AreaMapScript const& areaMapScript);
  void             setXMLComputationMask    (pcrxml::ComputationMask const& c);
  void             setMemoryExchangeData    (const ASTSymbolTable& symbols,
                                             void            **data);
  void             transferMemoryExchangeItemIntoDataTransferArray(
                                              MemoryExchangeItem* item);

  IoFieldStrategy& ioFieldStrategy          ();

  //! may throw com::Exception if can not be created
  FieldWriter*     createFieldWriter     (const ASTSymbolInfo&   s);
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const RunDirectory& runDirectory          () const;
  const AreaMap&      areaMap               () const;
  const Timer&        timer                 () const;

  bool                writeEachTimeStep     () const;
  const std::string&  debugMVAssignmentsMap () const;
  bool                mvCompression         () const;

  const geo::RasterSpace& rasterSpace       () const;

  void              debugMVAssignments      (const Field* f) const;

  Field*            createReadSpatial       (const std::string& mapName,
                                             VS vs) const;
  Field*            createReadField         (const std::string& mapName,
                                             const DataType&    type)const;
  GridStat          writeField              (const std::string& fileName,
                                             const Field *f) const;

  void              checkOutputFilePath     (ASTSymbolInfo const& i);
  StackInput*     createStackInput(const std::string& externalName, const MapStackType& type);
  std::string           outputFilePath(const std::string& fileName) const;
  DataType              resolveInputSymbol(std::string&           newExternalName,
                                           const        DataType& dt);
  Field*                createSpatial(VS vs) const;
  const SpatialPacking&  spatialPacking() const;


  std::string              makeStackItemName     (const std::string& iname,
                                                  int   atTimeStep) const;
  void                     setStackInfo          (const StackInfo& s) const;

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
