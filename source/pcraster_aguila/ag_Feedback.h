#ifndef INCLUDED_AG_FEEDBACK
#define INCLUDED_AG_FEEDBACK



#include <QtOpenGL>



namespace ag {
  class FeedbackPrivate;
}



namespace ag {



//! The Feedback class encapsulates the OpenGL feedback mechanism.
/*!
  This class can be used to start and stop OpenGL's feedback mode and capture
  its results.
*/
class Feedback
{

private:

  FeedbackPrivate* d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  Feedback&        operator=           (const Feedback&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Feedback            (const Feedback&);

#ifdef DEBUG_DEVELOP
  void             describe            () const;
#endif

public:

  typedef const GLfloat* const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Feedback            (GLint size);

  /* virtual */    ~Feedback           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             start               () const;

  void             stop                () const;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  GLint            nrValuesStored      () const;

  const GLfloat*   buffer              () const;

  const_iterator   begin               () const;

  const_iterator   end                 () const;

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
