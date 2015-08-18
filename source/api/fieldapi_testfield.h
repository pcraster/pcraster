#ifndef INCLUDED_FIELDAPI_TESTFIELD
#define INCLUDED_FIELDAPI_TESTFIELD



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace fieldapi {



//! template hack to define test fields for unit test.
/*!
   Use in combo with TEST_FIELD_INIT macro:
   <pre>
     UINT1  d1[ ] = { 0, 1, 2 , 3, 40, 5 };
     TestField<UINT1,2,3> d2(d1);
     ReadOnlySpatial<UINT1,UINT1> r(TEST_FIELD_INIT(d2));
     // or
     ReadWriteData<UINT1,UINT1> rw(TEST_FIELD_INIT(d2));
   </pre>
*/
template<class T, size_t d_nrRows, size_t d_nrCols>
  class TestField
{
     //!  1d array of all data
     T *d_data;
     //! 2d array with ptrs into d_data
     T *d_rows[d_nrRows];

  public:
     //! ctor
     TestField(T *data):
       d_data(data)
     {
       T *p=d_data;
       for (size_t r=0; r<d_nrRows; r++) {
         d_rows[r] = p;
         p += d_nrCols;
       }
     }

     //! return 2d array
     T** data() {
       return (T **)d_rows;
     }
     //! nr of rows
     size_t nrRows() const { return d_nrRows; }
     //! nr of cols
     size_t nrCols() const { return d_nrCols; }
};

#define TEST_FIELD_INIT(tf)  (tf).data(),(tf).nrRows(),(tf).nrCols()


//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace fieldapi

#endif
