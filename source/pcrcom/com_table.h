#ifndef INCLUDED_COM_TABLE
#define INCLUDED_COM_TABLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_COM_BASICTABLE
#include "com_basictable.h"
#define INCLUDED_COM_BASICTABLE
#endif



namespace com {
  class PathName;
}



namespace com {



/*!
  \class Table
  \brief The Table is for table objects with a description and variable
         names.

  The Table class is an extension of the com::BasicTable class. It adds
  a description and names for the colummns.
*/
class Table: public BasicTable
{

private:

  //! Description of the geo-eas table.
  std::string      d_descr;

  //! Names of the columns.
  std::vector<std::string> d_names;

  //! Assignment operator. NOT IMPLEMENTED.
  Table &          operator=           (const Table &);

  //! Copy constructor. NOT IMPLEMENTED.
                   Table               (const Table &);

  //! Initialises the table.
  void             init                ();

  //! Frees dynamically allocated memory.
  void             clean               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Table               ();

                   Table               (size_t nrCols,
                                        size_t nrRecs);

                   Table               (com::PathName const& fileName);

  //! Destructor.
  /* virtual */    ~Table              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setLabel            (size_t col,
                                        std::string const& label);

  friend std::istream &operator>>      (std::istream &s,
                                        Table &t);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  friend std::ostream &operator<<      (std::ostream &s,
                                        Table const& t);

  //! Returns the description.
  const std::string &descr             () const;

  //! Returns the name of variable \a i.
  const std::string &name              (size_t i) const;

  std::string const& label             (size_t col) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

  std::istream &operator>>      (std::istream& s,
                                 Table& t);
  std::ostream &operator<<      (std::ostream &s,
                                 Table const& t);


} // namespace com

#endif
