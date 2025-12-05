#ifndef INCLUDED_OLDCALC_MODELBUILDER
#define INCLUDED_OLDCALC_MODELBUILDER

#include "stddefx.h"
#include "calc_script.h"

#include <memory>



namespace calc {
  // ModelBuilder declarations.
  class FieldExpr;
  class UsePar;
  class Symbol;
  class LookupTable;
  class RunSettings;
}



namespace calc {



//! Build script bit by bit
class ModelBuilder
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ModelBuilder&           operator=           (const ModelBuilder&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ModelBuilder               (const ModelBuilder&);

  Script   d_script;

  UsePar   usePar(const std::string& par);
  Symbol   symbol(const std::string& name);

  //! name for \class calc::PositionName used to create symbols
  std::shared_ptr<std::string> d_positionName;

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
                                       FieldExpr*         expr,
                                       bool               write);
  FieldExpr*        addFieldExpr              (const std::string& expr);
  void              addStatement              (const std::string& statement, bool write=true);
  void              addStatement              (Statement* stat);

  void              addBinding                (const std::string& left,
                                               const std::string& right);
  void              evaluateBindings();

  void              addLookupTable            (const std::string& name,
                                               LookupTable       *table);
  calc::RunSettings parseExternalBindings     (const com::PathName& bindingFile);

  void              execute                   ();

  void              setClone                  (const std::string& clone);

  void              setMVCompression          (bool enable);

  void              set0Compression           (bool enable);

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
