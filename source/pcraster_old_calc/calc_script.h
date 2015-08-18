#ifndef INCLUDED_CALC_SCRIPT
#define INCLUDED_CALC_SCRIPT


#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif


#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_STATEMENTBLOCK
#include "calc_statementblock.h"
#define INCLUDED_CALC_STATEMENTBLOCK
#endif

#ifndef INCLUDED_CALC_RUNDIRECORY
#include "calc_rundirectory.h"
#define INCLUDED_CALC_RUNDIRECTORY
#endif
#ifndef INCLUDED_CALC_PROGRESSPULSE
#include "calc_progresspulse.h"
#define INCLUDED_CALC_PROGRESSPULSE
#endif
#ifndef INCLUDED_CALC_REPORTTABLE
#include "calc_reporttable.h"
#define INCLUDED_CALC_REPORTTABLE
#endif
#ifndef INCLUDED_CALC_BINDINGTABLE
#include "calc_bindingtable.h"
#define INCLUDED_CALC_BINDINGTABLE
#endif
namespace geo {
  class RasterSpace;
}

namespace calc {

class Report;
class ReportDefinition;
class ParsPar;
class UserSymbol;
class FieldMapInputParameter;
class SymbolTable;
class ArcViewExtCheckData;
class ProgressCallBack;

class IoFieldStrategy;

//! Script of pcrcalc statements
/*! Script is the central class, every instance of a model
    is done by creating a Script object and running it.

    By default nothing is printed to std::out or std::cerr. All
    errors leave the methods by exceptions. see setFileOutputStream()
    and setProgressCallBack() to enable printing results and progress
    info.

    \todo
      remove Multiple inheritance by making StatementBlock a member
 */
class Script:
  public StatementBlock,
  public IScript {
  public:
  typedef enum ExitValueType {
    //! -e option
    LAST_VAL,
    //! -E option
    EXIT_ON_0,
    //! just 0 on succes (default)
    ALWAYS_0
  } ExitValueType;

  IoFieldStrategy& ioFieldStrategyMod();

  void recheckIoFieldStrategy() const;

 private:
  /*!
    Why d_ioFieldStrategy is mutable: we parse the global option
     that can be present on the first #! line of script,
     too late, if present in script: can not do it in script-ctor
     anymore, but in Script::strategy() const

     \todo
       Strategy is een rommeltje zo

   */
   mutable IoFieldStrategy   *d_ioFieldStrategy;

   /*! ptr to external call back class
    */
   ProgressCallBack          *d_progressCallBack;
   ExitValueType              d_exitValueType;
   //! write value of file output constructs to this stream
   /*!
    *  if 0 no output
    */
   std::ostream              *d_fileOutputStream;

   //! if missing value compression is enabled
   bool d_compression;

   //! if 0 compression is enabled
   bool d_0compression;

   //! the value to exit/return application with
   int    d_exitVal;

   RunDirectory d_runDirectory;

  //! declare BEFORE d_symTab, d_symtab uses report in destruction
  ReportTable      d_reportTable;

  //! global symboltable
  /*!
      Must be allocated dynamically, so we can delete it BEFORE
      d_ioFieldStrategy; destructors of some symbols depend on
      d_ioFieldStrategy
   */
  SymbolTable      *d_symTab;

  //! write html version to this file
  /*! empty if no html file generation (default)
   */
  com::PathName d_htmlFile;

  //!  write xml info to this file
  /*! empty if no xml file generation (default)
   */
  com::PathName       d_xmlFile;

  //! is tss will be written to disk at each timestep
  bool d_writeEachTimeStep;

  //! file to write in case of debug assertion
  /*! empty if DebugMode is off (default)
   */
  com::PathName       d_debugMapName;

  //! linear UINT1 map, for mask
  unsigned char *d_areaMask;

  //! the compressor
  Compressor *d_compressor;

  //! are there one or more statements prefixed with report?
  bool       d_aReportFound;

  //! the report default clause
  Report *d_reportDefault;

  BindingTable d_bindingTable;

  //! 0 if not defined, name is also in d_cloneNameCommon
  FieldMapInputParameter *d_areaMap;

  void                    setupClone();

  void                    checkClone(const std::string& mapFileName);
  FieldMapInputParameter *detectExternalFieldLeaf(const ParsPar& par);
  bool                    updateExitVal(double val);


  com::PathName           d_scriptFileName;


 public:
  // CREATORS
  Script();

  virtual ~Script();

  // MANIPULATORS

  void setProgressCallBack(ProgressCallBack *progressCallBack);

  void setFileOutputStream(std::ostream* fileOutputStream);

  void setHtmlFile      (const std::string&  htmlFileName);
  void setXmlFile       (const std::string&  xmlFileName);
  void setScriptFileName(const com::PathName& scriptFileName);
  void setDebugMap      (const std::string&  debugMapName);
  void setWriteEachTimeStep(bool enable);
  void setMVCompression (bool enable);
  void set0Compression (bool enable);
  void setExitValueType (const ExitValueType exitValueType);
  void setRunDirectory  (const com::PathName&  runDirectory,
                         const com::PathName&  externalBindingsFile);

  void addReport        (const ReportDefinition *report);
  void addBinding(const Symbol& left, const Symbol& right, VS vs=VS_FIELD);
  void evaluateBindings();

  void addSymbol        (UserSymbol *sym);

  void setReportFound   ();
  void setAreaMap       (const Symbol&  name);
  void setTimer(size_t start, size_t end, size_t slice);
  void setTimer         (const Symbol& nameTss);


  //! check clone existence, types, print, execute
  void buildScript();

  void executeBlock();
  void run();

  void   processFileOutputValue(double val);
  FieldMapInputParameter *addExternalFieldLeaf(const ParsPar& par);

  Symbol                  generatedSymbol( const std::string& context,
                                           const std::string& name);

  const StackReader* createStackReader(const std::string& stackName);

  // ACCESSORS


  bool inDynamic() const;
  bool allIsWritten()const;
  const Report *reportDefault()const;

  void print(InfoScript& i)const;
  void htmlPrint() const;
  UserSymbol *findSymbol(const class Symbol* sym,
    VS typesExpected, bool mustExist) const;

  bool   writeEachTimeStep() const;
  bool  debugMvAssignments() const;
  bool  zeroCompression() const;
  std::string  debugMapName() const;
  unsigned char *areaMask() const;

  const Report      *findReport(const Symbol& u) const;
  const Symbol      *findBinding(const std::string& name) const;
  SubParameter      *findRightParameter( const ParsPar& par, VS expectedVs ) const;
  SubParameter      *findLeftParameter( const ParsPar& par, VS expectedVs) const;

  const geo::RasterSpace& rasterSpace() const;
  const Compressor&       compressor()  const;

  void removeOutputObject(const std::string& objName) const;
  GridMap *createMap(const std::string& d_fileName, VS vs) const;

  void updateProgress(ProgressPulse p, int step=-1);
  int exitVal() const;

  void setArcViewExtCheckData(std::vector<ArcViewExtCheckData>& r) const;

  bool   esriGridIO() const;
  const IoFieldStrategy& ioFieldStrategy() const;

  std::string  inputFilePath(const std::string& fileName) const;
  std::string outputFilePath(const std::string& fileName) const;
};

}

#endif
