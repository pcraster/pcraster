#ifndef INCLUDED_AG_DOMANAGER
#define INCLUDED_AG_DOMANAGER


#include <cstring>
#include <vector>



namespace ag {
  class DataObject;
}



namespace ag {



/*!
  \class DOManager
  \brief The DOManager class is for managing data objects.

  ...
*/
//       1         2         3         4         5         6         7         8
class DOManager
{

private:

  //! Collection with data objects.
  std::vector<ag::DataObject *> d_dataObjects;

  //! Assignment operator. NOT IMPLEMENTED.
  DOManager &      operator=           (const DOManager &);

  //! Copy constructor. NOT IMPLEMENTED.
                   DOManager           (const DOManager &);

protected:

  typedef std::vector<ag::DataObject *>::iterator iterator;
  typedef std::vector<ag::DataObject *>::const_iterator const_iterator;

  //! Returns a const iterator to the first data object.
  const_iterator   begin               () const;

  //! Returns an iterator to the first data object.
  iterator         begin               ();

  //! Returns a const iterator to the one-past-the-last data object.
  const_iterator   end                 () const;

  //! Returns an iterator to the one-past-the-last data object.
  iterator         end                 ();

  //! Creates a new Data Object and returns a pointer to it.
  ag::DataObject *newDataObject    ();

  //! Deletes data object \a o from the collection.
  void             deleteDataObject    (ag::DataObject *o);

  //! Copies \a o and returns a pointer to the copy.
  ag::DataObject *copyDataObject   (const ag::DataObject *rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   DOManager           ();

  //! Destructor.
  virtual          ~DOManager          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the number of data objects in the collection.
  size_t           nrDataObjects       ();

#ifdef DEBUG_DEVELOP
  void             checkIntegrity      ();
#endif

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
