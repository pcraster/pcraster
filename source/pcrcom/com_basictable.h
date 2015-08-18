#ifndef INCLUDED_COM_BASICTABLE
#define INCLUDED_COM_BASICTABLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif



namespace com {



/*!
  \class BasicTable
  \brief The BasicTable class is for objects storing tabular data.

  Very simple tables can be managed with BasicTable objects. The tables
  don't have descriptions, titles, variable names, etc. They just have columns
  with data.

  Only numerical data can be stored in a BasicTable object.
*/
class BasicTable
{

private:

  //! Iterator type for iterating over the columns. For internal use only.
  typedef std::vector<std::vector<double> *>::iterator col_iterator;

  //! Datastructure with all tabular data.
  std::vector<std::vector<double> *> d_columns;

  //! Number of records in the longest column.
  size_t           d_nrRecs;

  //! Assignment operator. NOT IMPLEMENTED.
  BasicTable &     operator=           (const BasicTable &);

  //! Copy constructor. NOT IMPLEMENTED.
                   BasicTable          (const BasicTable &);

  //! Frees dynamically allocated memory.
  void             clean               ();

protected:

  //! Create space for \a n columns of data.
  void             resize              (size_t n);

  void             resize              (size_t nrCols,
                                        size_t nrRecs);

public:

  //! Iterator type for iterating over the records of a column.
  typedef std::vector<double>::const_iterator const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   BasicTable          ();

                   BasicTable          (size_t nrCols,
                                        size_t nrRecs);

  //! Destructor.
  virtual          ~BasicTable         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Input operator for reading a BasicTable object from a stream.
  friend std::istream &operator>>      (std::istream &s,
                                        BasicTable &t);

  void             setValue            (size_t col,
                                        size_t rec,
                                        double value) const;

  void             setMV               (size_t col,
                                        size_t rec);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Output operator for writing a BasicTable object to a stream.
  friend std::ostream &operator<<      (std::ostream &s,
                                        const BasicTable &t);

  //! Returns the number of columns in the table.
  size_t           nrCols              () const;

  //! Returns the number of records in the longest column.
  size_t           nrRecs              () const;

  bool             isMV                (size_t col,
                                        size_t rec);

  bool             allMV               (size_t i) const;

  double           min                 (size_t i) const;

  double           max                 (size_t i) const;

  double           value               (size_t col,
                                        size_t rec) const;

  double&          value               (size_t col,
                                        size_t rec);

  //! Returns an iterator to the first cel of column \a i.
  const_iterator   begin               (size_t i) const;

  //! Returns an iterator to the one-past-the-last cel of column \a i.
  const_iterator   end                 (size_t i) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

} // namespace com

#endif
