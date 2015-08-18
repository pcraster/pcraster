#ifndef INCLUDED_COM_RCOBJECT
#define INCLUDED_COM_RCOBJECT



#include <cstring>



namespace com {

/*!
  \class RCObject
  \brief The RCObject class is a base class for reference counted objects.

  longer_description
*/
class RCObject
{

private:

  size_t           d_refCount;

  bool             d_shareable;

protected:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   RCObject            ();

  //! Copy constructor.
                   RCObject            (const RCObject &);

  //! Assignment operator.
  RCObject &       operator=           (const RCObject &);

  //! Destructor.
  virtual          ~RCObject           ();

public:

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addReference        ();

  void             removeReference     ();

/*
  void             markUnshareable     ();
*/

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

/*
  bool             isShareable         () const;
*/

  bool             isShared            () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace com

#endif
