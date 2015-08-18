#ifndef INCLUDED_CALC_MODELBUILDER
#define INCLUDED_CALC_MODELBUILDER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_RUNTIMEENVSETTINGS
#include "calc_runtimeenvsettings.h"
#define INCLUDED_CALC_RUNTIMEENVSETTINGS
#endif



namespace com {
  class PathName;
}

namespace calc {

class ASTExpr;
class ASTScript;
class UsePar;
class Symbol;
class LookupTable;
class RunSettings;

//! \brief Build script bit by bit
class ModelBuilder
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ModelBuilder&           operator=           (const ModelBuilder&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ModelBuilder               (const ModelBuilder&);

  RunTimeEnvSettings  d_rtes;
  //! FTTB only a single assignment in support ManualExampleTester::test
  ASTScript   *d_script;

  UsePar   usePar(const std::string& par);
  Symbol   symbol(const std::string& name);

  //! name for \class calc::PositionName used to create symbols
  boost::shared_ptr<std::string> d_positionName;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ModelBuilder               ();

  /* virtual */    ~ModelBuilder              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void                 setPositionName(const std::string& name);

  void                 setGlobalOption(const std::string& option);

  void              addFieldAssignment(const std::string& par,
                                       ASTExpr*         expr,
                                       bool               write);
  ASTExpr*        addASTExpr              (const std::string& expr);
//void              addStatement     (const std::string& statement, bool write=true);
  void              setModel         (const std::string& model);

  void              addBinding                (const std::string& left,
                                               const std::string& right);
  void              evaluateBindings();

  void              addLookupTable            (const std::string& name,
                                               LookupTable       *table);
  void              execute                   ();

  void              setClone                  (const std::string& clone);

  void              setMVCompression          (bool enable);
  void              setCompile                (bool enable);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
