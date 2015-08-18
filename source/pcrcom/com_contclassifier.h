#ifndef INCLUDED_COM_CONTCLASSIFIER
#define INCLUDED_COM_CONTCLASSIFIER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif



//namespace pack {



/*!
  \class com_ContClassifier
  \brief The com_ContClassifier class is for objects holding classification
         properties of continuous data.
*/
template<class T>
class com_ContClassifier
{

private:

  //! The minimum value.
  T                d_min;

  //! The maximum value.
  T                d_max;

  //! The minimum cutoff value.
  T                d_minCutOff;

  //! The maximum cutoff value.
  T                d_maxCutOff;

  //! The number of classes.
  size_t           d_nc;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
                   com_ContClassifier  ();

  //! Constructor.
                   com_ContClassifier  (T min,
                                        T max,
                                        size_t nc);

  //! Destructor.
                   ~com_ContClassifier ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the minimum value to \a v.
  void             setMin              (T v);

  //! Sets the maximum value to \a v.
  void             setMax              (T v);

  //! Sets the minimum cutoff value to \a v.
  void             setMinCutOff        (T v);

  //! Sets the maximum cutoff value to \a v.
  void             setMaxCutOff        (T v);

  //! Sets the number of classes to \a nc.
  void             setNrClasses        (size_t nc);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the minimum value.
  T                min                 () const;

  //! Returns the maximum value.
  T                max                 () const;

  //! Returns the minimum cutoff value.
  T                minCutOff           () const;

  //! Returns the maximum cutoff value.
  T                maxCutOff           () const;

  //! Returns the number of classes.
  size_t           nrClasses           () const;

  //! Returns the class index of \a v.
  size_t           classIndex          (T v) const;

  //! Returns the classwidth.
  T                classWidth          () const;

  //! Returns the upper class border of class with index \a i.
  T                classBorder         (size_t i) const;

  //! Returns if \a classifier is equal to the caller.
  bool             equals              (const com_ContClassifier &c) const;

  //! Returns the string representation of the class border of class i.
  std::string      descr               (size_t i) const;

};



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

template <class T>
bool               operator==          (const com_ContClassifier<T> &lhs,
                                        const com_ContClassifier<T> &rhs);

template <class T>
bool               operator!=          (const com_ContClassifier<T> &lhs,
                                        const com_ContClassifier<T> &rhs);

//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------



//} // namespace pack

#endif
