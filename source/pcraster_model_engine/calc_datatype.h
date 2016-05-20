#ifndef INCLUDED_CALC_DATATYPE
#define INCLUDED_CALC_DATATYPE



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
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif
#ifndef INCLUDED_CALC_DIMENSION
#include "calc_dimension.h"
#define INCLUDED_CALC_DIMENSION
#endif
#ifndef INCLUDED_CALC_MAPSTACKTYPE
#include "calc_mapstacktype.h"
#define INCLUDED_CALC_MAPSTACKTYPE
#endif



namespace calc {
  // DataType declarations.
}



namespace calc {



//! Type of field or other type, not only field types!
/*!
 */
class DataType
{

private:
  //! current set of possible (o)vs's
  VS  d_vs;

  /**
   * \defgroup Specifics specific member for certain d_vs values
   */
  /*@{*/

  //! spatial states possible. d_vs in [VS_FIELD]
  ST d_st;

  //! true if \a this allows restrict() to allow ST promotion
  bool             d_allowPromotion;

  //! specialization d_vs == VS_S (OpenMI)
  Dimension        d_unit;
  /*! vs of result for timeinput* DataStorage's:
   *      d_vs() in [VS_MAPSTACK, VS_TSS]
   */
  VS               d_resultType;

  //! type of StackInput d_vs == VS_MAPSTACK
  MapStackType     d_mapStackType;

  //! types (VS_FIELD) for lookuptables. d_vs==VS_TABLE
  std::vector<VS>  d_tableColTypes;


  /*@}*/

  void init();

  ST intersect(ST state) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataType               ();
                   DataType               (VS vs);
                   DataType               (VS vs, ST st);
                   DataType               (VS vs, bool spatial);
          explicit DataType               (const double& value);
                   DataType               (DataType const& rhs);

  DataType&        operator=              (DataType const& rhs);

  /* virtual */    ~DataType              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             restrict            (VS req);
  void             restrict            (const DataType& req);

  void             setAllowPromotion   (bool allowPromotion);
  void             promoteToSpatial    ();
  void             setUnit             (const Dimension& unit);

  void             setResultType       (VS symbolType,VS resultType);
  void             setTableColTypes    (const std::vector<VS>& tableColTypes);
  void             setMapStackType     (const MapStackType& mapStackType);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool                   allowPromotion        () const;
  VS                     resultType            () const;
  const std::vector<VS>& tableColTypes         () const;
  const MapStackType&    mapStackType          () const;

  ST                     st                    () const;
  VS                     vs                    () const;
  const Dimension&       unit                  () const;

  bool                   stNonSpatial          () const;
  bool                   stSpatial             () const;
  bool                   stEither              () const;
  bool                   singleVs              () const;
  bool                   isField               () const;

  static void            restrict              (VS& vs, VS req);

};


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------


bool operator==(const DataType& e1,
                const DataType& e2);

bool operator!=(const DataType& e1,
                const DataType& e2);

std::ostream &operator<<(std::ostream& s, const DataType& ft);

} // namespace calc

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------




#endif
