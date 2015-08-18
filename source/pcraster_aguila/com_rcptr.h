#ifndef INCLUDED_COM_RCPTR
#define INCLUDED_COM_RCPTR





namespace com {



/*!
  \class   RCPtr
  \brief   The RCPtr is a template class for smart pointers-to-T objects.
  \warning The operator void*() member allows for comparison between
           RCPtr<appels> and RCPtr<peren>. Don't do that!

  T must inherit from RCObject.
*/
template<class T>
class RCPtr
{

private:

  T *              d_pointee;

  void             init                ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   RCPtr               (T *realPtr = 0);

  //! Assignment operator.
  RCPtr &          operator=           (const RCPtr &rhs);

  //! Copy constructor.
                   RCPtr               (const RCPtr &rhs);

  //! Destructor.
                   ~RCPtr              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  T *              operator->          () const;

  T &              operator*           () const;

                   operator void *     () const;

  //! Returns the pointee. Be careful! Do not delete!
  T *              ptr                 () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

template<class T>
inline T *RCPtr<T>::operator->() const
{ return d_pointee; }

template<class T>
inline T &RCPtr<T>::operator*() const
{ return *d_pointee; }

template<class T>
inline RCPtr<T>::operator void *() const
{ return (d_pointee != 0) ? (void *)d_pointee : (void *)0; }

template<class T>
inline T *RCPtr<T>::ptr() const
{ return d_pointee; }



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace com

#endif
