#ifndef INCLUDED_OLDCALC_SCRIPT
#define INCLUDED_OLDCALC_SCRIPT

#include "com_pathname.h"
#include "calc_iscript.h"
#include "calc_statementblock.h"
#include "calc_rundirectory.h"
#include "calc_progresspulse.h"
#include "calc_reporttable.h"
#include "calc_bindingtable.h"

#include <vector>



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
struct ArcViewExtCheckData;
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
   ExitValueType              d_exitValueType{ALWAYS_0};
   //! write value of file output constructs to this stream
   /*!
    *  if 0 no output
    */
   std::ostream              *d_fileOutputStream{nullptr};

   //! if missing value compression is enabled
   bool d_compression{false};

   //! if 0 compression is enabled
   bool d_0compression{false};

   //! the value to exit/return application with
   int    d_exitVal{0};

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
  bool d_writeEachTimeStep{false};

  //! file to write in case of debug assertion
  /*! empty if DebugMode is off (default)
   */
  com::PathName       d_debugMapName;

  //! linear UINT1 map, for mask
  unsigned char *d_areaMask{nullptr};

  //! the compressor
  Compressor *d_compressor{nullptr};

  //! are there one or more statements prefixed with report?
  bool       d_aReportFound{false};

  BindingTable d_bindingTable;

  //! 0 if not defined, name is also in d_cloneNameCommon
  FieldMapInputParameter *d_areaMap{nullptr};

  void                    setupClone();

  void                    checkClone(const std::string& mapFileName) override;
  FieldMapInputParameter *detectExternalFieldLeaf(const ParsPar& par);
  bool                    updateExitVal(double val);


  com::PathName           d_scriptFileName;


 public:
  // CREATORS
  Script();

  ~Script() override;

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

  void addSymbol        (UserSymbol *sym) override;

  void setReportFound   () override;
  void setAreaMap       (const Symbol&  name);
  void setTimer(size_t start, size_t end, size_t slice);
  void setTimer         (const Symbol& nameTss);


  //! check clone existence, types, print, execute
  void buildScript();

  void executeBlock() override;
  void run() override;

  void   processFileOutputValue(double val) override;
  FieldMapInputParameter *addExternalFieldLeaf(const ParsPar& par) override;

  Symbol                  generatedSymbol( const std::string& context,
                                           const std::string& name);

  const StackReader* createStackReader(const std::string& stackName) override;

  // ACCESSORS


  bool inDynamic() const override;
  bool allIsWritten()const override;
  const Report *reportDefault()const override;

  void print(InfoScript& i)const override;
  void htmlPrint() const;
  UserSymbol *findSymbol(const class Symbol* sym,
    VS typesExpected, bool mustExist) const override;

  bool   writeEachTimeStep() const override;
  bool  debugMvAssignments() const override;
  bool  zeroCompression() const override;
  std::string  debugMapName() const override;
  unsigned char *areaMask() const override;

  const Report      *findReport(const Symbol& u) const;
  const Symbol      *findBinding(const std::string& name) const override;
  SubParameter      *findRightParameter( const ParsPar& par, VS expectedVs ) const override;
  SubParameter      *findLeftParameter( const ParsPar& par, VS expectedVs) const override;

  const geo::RasterSpace& rasterSpace() const override;
  const Compressor&       compressor()  const override;

  void removeOutputObject(const std::string& objName) const override;
  GridMap *createMap(const std::string& d_fileName, VS vs) const override;

  void updateProgress(ProgressPulse p, int step=-1) override;
  int exitVal() const;

  void setArcViewExtCheckData(std::vector<ArcViewExtCheckData>& r) const;

  bool   esriGridIO() const override;
  const IoFieldStrategy& ioFieldStrategy() const override;

  std::string  inputFilePath(const std::string& fileName) const override;
  std::string outputFilePath(const std::string& fileName) const override;
};

}

#endif
