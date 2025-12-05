#ifndef INCLUDED_DAL_TEXTMATRIXDRIVER
#define INCLUDED_DAL_TEXTMATRIXDRIVER

#include "dal_MatrixDriver.h"
#include "dal_TextFileDriver.h"



namespace dal {
  // TextMatrixDriver declarations.
}



namespace dal {



//! This class implements an i/o driver for text formatted matrix datasets.
/*!
  This driver assumes the matrix is stored as text formatted in a grid of
  values separated by white space. The type of the values in the whole matrix
  must be constant.
*/
class TextMatrixDriver: public MatrixDriver,
                        public TextFileDriver
{

  friend class TextMatrixDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  TextMatrixDriver& operator=          (TextMatrixDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   TextMatrixDriver    (TextMatrixDriver const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TextMatrixDriver    ();

  /* virtual */    ~TextMatrixDriver   () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  using MatrixDriver::read;

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  Matrix*          open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  DataSpace        dataSpace           (std::string const& name) const;

  DataSpace        dataSpace           (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  void             read                (std::string const& name,
                                        Matrix& matrix) const override;

  Matrix*          read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

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



} // namespace dal

#endif
