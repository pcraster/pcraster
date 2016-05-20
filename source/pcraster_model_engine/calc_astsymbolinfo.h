#ifndef INCLUDED_CALC_ASTSYMBOLINFO
#define INCLUDED_CALC_ASTSYMBOLINFO



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif
#ifndef INCLUDED_CALC_OVS
#include "calc_ovs.h"
#define INCLUDED_CALC_OVS
#endif
#ifndef INCLUDED_CALC_DEFINITIONROLE
#include "calc_definitionrole.h"
#define INCLUDED_CALC_DEFINITIONROLE
#endif
#ifndef INCLUDED_CALC_IOTYPE
#include "calc_iotype.h"
#define INCLUDED_CALC_IOTYPE
#endif

namespace pcrxml {
  class Relation;
  class Definition;
}

namespace com {
  class Exception;
  class PathName;
}

namespace calc {

  class LinkInExpr;
  class StackInput;
  class ASTPar;
  class ASTNumber;
  class Position;
  class Report;
  class ArrayDefVector;
  class IOStrategy;
  class SymException;
  class VSClash;
  class ASTDefinition;

//! where is a symbol written
enum  ReportPosition {
  RPInitial,  /*!< in a static script or initial section */
  RPDynamic,  /*!< in the dynamic section */
  RPNone      /*!< not reported */
};

//! Information needed/collected
/*!
    - d_firstCodePresence, d_firstAss are set in BuildTypesVisitor
    - d_hasInterface, d_definitionRole, d_description in setInfo

    \todo
      d_firstCodePresence, d_report, d_reportPar,d_firstAss are not owned ptrs, if not used then OK as in
      python link (no report, no assignment), otherwise their targets can not go out of
       scope: code AST must kept alive

    spos: does member define's a script position (e.g. has \class Position somewhere as a sub-member)
          - None: no position info
          - Code: relative to code body, ASTScript::d_code
 */
class ASTSymbolInfo
{
private:
  /**
   *  \defgroup CodeAST  Information from the actual code AST
   *                     for this symbol
   */
  /*@{*/

  /*!\brief position where symbol has its first presence in the code.
   *
   * With code presence we mean in the ASTScript::d_code.
   *
   * This value is ALWAYS defined, but reveals programming error
   * if it maps to calc::detail::parDefaultCtor.
   *
   * \note pointer is not owned here
   */
  const ASTPar      *d_firstCodePresence;

  //! first presence on the left hand side of an assignment (ASTAss)
  /*!
   * In other words is there at some point a value assigned to this symbol in
   * the code?  0, if never a value assigned.
   * \note pointer is not owned here
   *
   * Set from BuildTypesVisitor and only used in setConstantByBinding(ASTNumber *n).
   */
  const ASTPar*       d_firstAss;

  //! the ASTPar node tagged as reported.
  /*!
   *  lhs-ASTPar as in <pre>d_reportPar = ...expr ...;</pre>.
   *  unique point where value is written (reported)
   *
   *  If d_report is 0 then d_reportPar is meaningless.
   *
   *  This pointer is not owned.
   *
   *  spos: Code
   */
  const ASTPar*      d_reportPar;
  //! 0 if not written
  /*!
   * this pointer is not owned
   *
   *  spos: Code or default report
   */
  const Report*      d_report;

  /*@}*/

  /**
   *  \defgroup KnownValues Values known during parsing
   *            only one of these or none can be set
   *            lookupTable() is also one of the values
   */
  /*@{*/

  //! created in resolve() when nrTimeStepsToCheck > 0 for VS_MAPSTACK
  StackInput*         d_stackInput;

  /*! \brief created in BuildTypesVisitor() when ctor found for this symbol VS_OBJECT
   *  not owned
   */
  LinkInExpr*         d_objectLinkConstructor;

  /*@}*/

  bool                d_hasInterface;
  /*!
   * spos: None
   */
  DefinitionRole      d_definitionRole;
  //! see Variable concept
  IOType              d_ioType;

  /*!
   * \note d_dataType is also modelled within d_definition. But it lives in d_dataType:
   *       - setDefinition will pass d_definition/DataType to d_dataType
   *       - createDefinition will put d_dataType in new Definition
   */
  DataType            d_dataType;

  /*!
   * spos: None
   */
  ReportPosition      d_reportPosition;

  //! how is definition created, if existing
  enum DefinitionCreation {
    NotCreated,
    Binding,
    Interface,
    Definition,
    Resolve
  };
  DefinitionCreation  d_definitionCreation;
  pcrxml::Definition* d_definition;

  void               init                     ();
  void               initCopy                 (const ASTSymbolInfo& rhs);

  std::string        errorMsgName             () const;
  bool               firstIsCreation          () const;

  static const char *str                      (DefinitionCreation dc);
  void               resetDefinition          (DefinitionCreation definitionCreation);
  void               setResolvedScriptOutput  (std::string const& foundFile);

  friend std::ostream &operator<<(std::ostream& s, ASTSymbolInfo const& si);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   ASTSymbolInfo               (const DataType& dataType,
                                                const ASTPar *first);
                   ASTSymbolInfo               ();

  //! Assignment operator.
  ASTSymbolInfo&           operator=           (const ASTSymbolInfo&);

  //! Copy constructor.
                   ASTSymbolInfo               (const ASTSymbolInfo&);

  /* virtual */    ~ASTSymbolInfo              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  // { called from BuildTypesVisitor

  DataType&        dataType                   ();
  void             setFirstAss                (const ASTPar *firstAss);
  void             setFirstIsCreation         ();
  void             setObjectLinkConstructor   (LinkInExpr *objectLinkConstructor);

  // }

  // { called from ASTScript::applyInterface()
  //   these calls are mutually exclusive:
  //     - setConstantByBinding:     binding a = scalar(3);
  //     - setExternalNameByBinding: binding a = a.map;
  //     - setInfo:         interface a {
  //                        }

  void             setConstantByBinding       (const ASTNumber *n);
  void             setExternalNameByBinding   (const std::string& externalName);
  void             setInfo                    (const ASTDefinition& ib);

  // }


  // called from XMLScriptClientInterface, mutual exclusive to call from
  // ASTScript::applyInterface() mentioned above
  void             setDefinition              (const pcrxml::Definition& d);


  // called from ASTScript::analyzeNoContext(), but where called does not matter
  // TODO could be used to adjust scriptInput/scriptOutput settings
  void             setIoType                  (const IOType& ioType);

  // called from ASTScript::setReports()
  void             setReport                  (const ASTPar *reportPar,
                                               const Report *report,
                                               bool inDynamic,
                                               bool xmlScriptInterface);


  void             resolve                    (IOStrategy&         ios);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool                   operator<             (const ASTSymbolInfo& rhs)const;

  std::string            firstText             () const;

  VS                     vs                    () const;
  OVS                    ovs                   () const;
  const DataType&        dataType              () const;
  const IOType&          ioType                () const;

  LinkInExpr*            objectLinkConstructor () const;
  double                 constantValue         () const;
  pcrxml::Relation const* lookupTable          () const;
  bool                   isConstant            () const;

  const StackInput*      stackInput            () const;

  std::string            externalName          () const;
  const std::string&     name                  () const;
  std::string            description           () const;


  const ASTPar*          reportPar             () const;
  ReportPosition         reportPosition        () const;
  const Report*          report                () const;

  std::string            vsClashError          (const VSClash&  v,
                                                const std::string&  currentVerb) const;
  void                   throwAtFirst          (const com::Exception& e) const;
  void                   throwSym              (const SymException& s) const;
  void                   throwSym              (const Position&      pos,
                                                const std::string& msg) const;
  void                   checkOneOutputVs      () const;

  pcrxml::Definition*    createDefinition      () const;

  size_t                 memoryInputId         () const;
  size_t                 memoryOutputId        () const;
  static size_t          noMemoryExchangeId    ();

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------





//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------


std::ostream &operator<<(std::ostream& s, ASTSymbolInfo const& si);


} // namespace calc

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------


#endif
