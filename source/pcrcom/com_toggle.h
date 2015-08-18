#ifndef INCLUDED_COM_TOGGLE
#define INCLUDED_COM_TOGGLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



//namespace pack {



/*!
  \class com_Toggle
  \brief The com_Toggle class is a base class for objects which can be enabled
         (active, visible, etc) and disabled (passive, hidden, etc).

  This is just a simple mixin class which provides a common interface for
  objects which have a 'toggleble' state.
*/
//       1         2         3         4         5         6         7         8
class com_Toggle
{

private:

  //! Object is enabled or not.
  bool             d_enabled;

  //! Frees dynamically allocated memory.
  void             clean               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   com_Toggle          (bool enabled = true);

  //! Destructor.
  virtual          ~com_Toggle         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the object to enabled if \a s == true.
  void             setEnabled          (bool s);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns if the object is enabled (true) or disabled.
  bool             enabled             () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//} // namespace pack

#endif
