#ifndef INCLUDED_CALC_ASTSCRIPT
#define INCLUDED_CALC_ASTSCRIPT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_REPORTTABLE
#include "calc_reporttable.h"
#define INCLUDED_CALC_REPORTTABLE
#endif
#ifndef INCLUDED_CALC_BINDINGTABLE
#include "calc_bindingtable.h"
#define INCLUDED_CALC_BINDINGTABLE
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
#ifndef INCLUDED_CALC_RUNTIMEENVSETTINGS
#include "calc_runtimeenvsettings.h"
#define INCLUDED_CALC_RUNTIMEENVSETTINGS
#endif
#ifndef INCLUDED_CALC_ASTDEFINITION
#include "calc_astdefinition.h"
#define INCLUDED_CALC_ASTDEFINITION
#endif


namespace geo {
  class RasterSpace;
}
namespace pcrxml {
  class Script;
}

namespace calc {

class ASTNode;
class ASTId;
class CFGNode;
class PointCodeBlockDll;
class Timer;
class IOStrategy;

/*!
 * ASTScript has a number of set... and transfer.. methods to construct
 * a script yielding binding, areamap, code, timer, report-definitions
 * and possible some  RunTimeEnvSettings apearing in the script.
 *
 * After construction the script can be analyzed and checked: analyzeNoContext(), analyzeAndResolve()
 *
 * ASTScript is illnamed. must become new Script but refactored
 */
class ASTScript
{
  // all test classes
  friend class P5Stack; // for compile

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ASTScript&           operator=           (const ASTScript& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ASTScript               (const ASTScript& rhs);

  //! code of the model (initial and dynamic section)
  ASTNode*         d_code;

  //! cfg of code
  CFGNode*         d_cfgCode;

  //! cfg of body
  /*! body is areamap, timer, code
   */
  CFGNode*         d_cfgBody;

  /*!
   *  - initialized with --clone if present
   *  - overwritten if areamap section is parsed
   */
  ASTId*           d_areaMap;

  //! external timer, if present (not 0) it takens precedence over parsed timer
  Timer*           d_externalTimer;


  //! the parsed timer from script
  /*! if d_timerEnd and d_timerStep empty then d_timerStartOrTss is a tss
   */
  ASTId           *d_timerStartOrTss, *d_timerEnd, *d_timerStep;

  //! does d_code contains a dynamic section?
  bool             d_containsDynamicSection;

  //! there are 1 or more statements prefixed by the report keyword
  bool             d_hasStatementWithReportKeyword;


  //! a possible point code dll
  PointCodeBlockDll* d_pointCodeBlockDll;

  RunTimeEnvSettings d_rteSettings;

  bool             d_reportOnlyForXMLScriptOutput;

  void             init             ();
  void             setAreaMapFromString(std::string const& value,
                                     std::string const& positionText);

  void             rebuildCFG       ();
  void             compile          ();
  void             setReports       ();

  void             callWithClashRewrite(void (ASTScript::*f)() );
  void             analyzeNoContextUnChecked();
  void             resolveUnChecked();

  typedef std::vector<ASTDefinition> Interface;

  ASTSymbolTable   d_symbols;
  BindingTable     d_bindings;
  Interface        d_interface;
  ReportTable      d_reports;

  Timer timer() const;

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ASTScript               ();

  /* virtual */    ~ASTScript              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              setRteSettings         (RunTimeEnvSettings const& rteSettings);
  void              setMVCompression       (bool enable);

  void              setReportOnlyForXMLScriptOutput(bool reportOnlyForXMLScriptOutput);


  void              transferCode           (ASTNode *code);
  void              transferAreaMap        (ASTId* areaMap);
  void              transferTimerSection   (ASTId* timerStartOrTss,
                                            ASTId* timerEnd,
                                            ASTId* timerStep);

  void              transferOneBinding     (ASTNode* bindingDefinition);
  void              addReportDefinition    (Report const& reportDef);
  void              addInterfaceDefinition (ASTDefinition const& astDef);

  void              setExternalTimer       (Timer const& externalTimer);

  void              analyzeNoContext       ();
  void              analyzeAndResolve      ();
  void              resolve                ();


  void              setSettingsFromXML     (pcrxml::Script const& script);
  ASTSymbolTable&   symbols                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  CFGNode*              cfgCode            () const;
  ASTNode*              astCode            () const;

  const RunTimeEnvSettings& rteSettings    () const;

  const ASTSymbolTable& symbols            () const;
  const BindingTable&   bindings           () const;
  const ReportTable&    reports            () const;

  bool             containsDynamicSection  () const;

  void             buildTypesFullClosure  ();
  void             applyInterface   ();
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
