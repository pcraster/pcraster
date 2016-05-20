#ifndef INCLUDED_CALC_DATATYPECLASH
#define INCLUDED_CALC_DATATYPECLASH



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif

namespace calc {
  // DataTypeClash declarations.
}



namespace calc {

//! base class for exceptions related to DataType clashes
class DataTypeClash
{

private:


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  // Assignment operator. DEFAULT
  // DataTypeClash&     operator=    (DataTypeClash const& rhs);

  //  Copy constructor. DEFAULT
  //     DataTypeClash               (DataTypeClash const& rhs);

                   DataTypeClash               ();

  /* virtual */    ~DataTypeClash              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};

/*!
    \brief something is required to be non spatial but a
   spatial is found or vice versa
 */
class STClash : public DataTypeClash {
    bool d_spatialFound;
  public:
    STClash(bool spatialFound):
      d_spatialFound(spatialFound)
    {};
    bool spatialFound() const {
      return d_spatialFound;
    }
};

class VSClash : public DataTypeClash {
    VS d_isOneOf,d_mustBeOneOf;
  public:
    VSClash(VS isOneOf, VS mustBeOneOf):
      d_isOneOf(isOneOf),d_mustBeOneOf(mustBeOneOf)
    {};
    VS isOneOf() const {
      return d_isOneOf;
    }
    VS mustBeOneOf() const {
      return d_mustBeOneOf;
    }
};

class TableClash : public DataTypeClash {
    std::string d_message;
  public:
    TableClash(size_t hasSize, size_t mustHaveSize);
    TableClash(size_t offendingColNr,
               const  VSClash& c);

    const std::string& message() const;
    static void checkNrOfColumns(size_t hasSize, size_t mustHaveSize);
};

class MapStackClash : public DataTypeClash {
    std::string d_message;
  public:
    MapStackClash(const std::string& message);
    const std::string& message() const;
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
