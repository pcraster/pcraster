#ifndef INCLUDED_CALC_STACKINPUT
#define INCLUDED_CALC_STACKINPUT

#include "stddefx.h"
#include "calc_datatype.h"
#include "calc_datavalue.h"

#include <vector>
#include <string>


namespace calc {
  // StackInput declarations.
  class MapStackType;
}



namespace calc {




class IOStrategy;
class IoFieldStrategy;
class Field;
class Timer;

//! for VS_MAPSTACK
/*!
 * \todo
     eigenlijk twee acties:
        - resolve: check type of input maps and mapstacks
                     door de eerste te vinden.
        - load: check completeness of mapstacks (timeinput/timeinputsparse)
                 en allemaal hetzelfde type (gebeurt nu nog in resolve)
    Implementeer alles als sparse FTTB.
 */
class StackInput : public DataValue {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  StackInput&           operator=           (const StackInput& rhs);


                   StackInput               ();

  std::string      d_stackName;

  IOStrategy&      d_fios;
  IoFieldStrategy& d_fs;

  //! holds index for each timestep
  std::vector<size_t> d_itemToLoad;

  //! type of the stack
  DataType d_type;

  Field*          d_cachedField;

  size_t          d_cachedTimeStep;

  std::string     name       (size_t t) const;
  VS              checkMap   (size_t i);
  bool            exists     (size_t t) const;
  Field          *readField  (size_t i) const;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


  /* virtual */    ~StackInput              () override;


                   StackInput               (IOStrategy&        fios,
                                             const std::string& stackName,
                                             const MapStackType& type);

                   StackInput               (const StackInput& rhs);


  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  Field*           read                      (size_t t);
  Field*           readLookup                (int    t);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  VS               fieldReturnVs             () const;
  OVS              ovs                       () const override;

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
