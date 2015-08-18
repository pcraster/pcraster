#ifndef INCLUDED_GEO_COLUMNFILE
#define INCLUDED_GEO_COLUMNFILE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif



//namespace pack {



/*!
  \class geo_ColumnFile
  \brief The geo_ColumnFile class is for reading columns of double values
         from ascii files.

  Two types of column files are supported: the obvious column file format
  without a header, just columns of doubles and the geo_eas format with a
  header describing the number of variables and the names of the variables.
  To check if a file has the geo-eas format you can use geoEasFormat().

  The class supports incremental reading of the file. First the header can
  be read, by calling readHeader(), to determine the title of the data
  file (title()), the number of variables in the file (nrVars()) and/or the
  names of the variables (varName()).

  After reading the header the data can be read using readData(). Normally
  all the columns are read but you can make a selection by calling selVar().

  If the (selected) columns have been read, then the data can be accessed
  through iterators (begin(), end()).

  If you want to read the file in one time you can call read().

  Variable/column numbers start with 1.
*/
//       1         2         3         4         5         6         7         8
class geo_ColumnFile
{

private:

  //! State values.
  enum State { INITIAL, HEADER_READ, DATA_READ };

  //! State of object.
  State            d_state;

  //! Filename of geo-eas file.
  std::string      d_fn;

  //! Filestream of column file.
  std::ifstream    d_fs;

  //! Number of variables/columns in column file.
  size_t           d_nrVars;

  //! Names of the variables.
  std::vector<std::string> d_varNames;

  //! Column numbers to read (x, y, v). The first column is 1.
  std::set<size_t> d_selVars;

  //! Title of the column file.
  std::string      d_title;

  //! Data.
  std::vector<std::vector<double> *> d_data;

  //! Auto delete mode.
  bool             d_ad;

  //! Assignment operator. NOT IMPLEMENTED.
  geo_ColumnFile & operator=           (const geo_ColumnFile &);

  //! Copy constructor. NOT IMPLEMENTED.
                   geo_ColumnFile      (const geo_ColumnFile &);

  //! Opens the file stream.
  void             open                ();

  //! Returns if the file has the geo-eas format (with a header).
  bool             geoEasFormat        (std::ifstream &fs) const;

  //! Initialises the class: discards all settings and data.
  void             initialise          ();

public:

  //! Iterator type.
  typedef std::vector<double>::const_iterator const_iterator;
  // typedef const double * const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   geo_ColumnFile      (bool autoDelete = true);

  //! Constructor takes filename \a n.
                   geo_ColumnFile      (const std::string &fn,
                                        bool autoDelete = true);

  //! Destructor.
                   ~geo_ColumnFile     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the filename.
  void             setFilename         (const std::string &fn);

  //! Reads the geo_eas column file with the current settings.
  void             read                ();

  //! Reads the header of the file.
  void             readHeader          ();

  //! Reads the data of the file.
  void             readData            ();

  //! Selects a column from the file.
  void             selVar              (size_t n);

  //! Sets the auto delete mode.
  void             setAutoDelete       (bool d);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the title of the geo-eas file.
  const std::string &title             () const;

  //! Returns the number of variables/columns in the column file.
  size_t           nrVars              () const;

  //! Returns the number of selected variables/columns in the column file.
  size_t           nrSelVars           () const;

  //! Returns the number of records read.
  size_t           nrRecs              () const;

  //! Returns the name of variable \a n.
  const std::string &varName           (size_t n) const;

  //! Returns if the file has the geo-eas format (with a header).
  bool             geoEasFormat        () const;

  //! Returns an iterator to the first value of variable \a n.
  const_iterator   begin               (size_t n) const;

  //! Returns an iterator to the 'one past the last' value of variable \a n.
  const_iterator   end                 (size_t n) const;

  //! Returns the vector with data for variable \a n.
  std::vector<double> *data            (size_t n) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//} // namespace pack

#endif
