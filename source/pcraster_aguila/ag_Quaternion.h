#ifndef INCLUDED_AG_QUATERNION
#define INCLUDED_AG_QUATERNION



#include <QtOpenGL>



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!

  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
//       1         2         3         4         5         6         7         8
class Quaternion
{

private:

  GLfloat          d_val[4];

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Quaternion          ();

                   Quaternion          (const Quaternion& q);

                   Quaternion          (GLfloat x,
                                        GLfloat y,
                                        GLfloat z);

                   Quaternion          (GLfloat angle,
                                        GLfloat x,
                                        GLfloat y,
                                        GLfloat z);

  /* virtual */    ~Quaternion         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Quaternion&      operator=           (const Quaternion& q);

  void             normalize           ();

  void             reset               ();

  void             postMult            (const Quaternion& q);

  void             multAndSet          (const Quaternion& q1,
                                        const Quaternion& q2);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             matrix              (GLfloat m[16]);

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
