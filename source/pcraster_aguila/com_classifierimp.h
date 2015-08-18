#ifndef INCLUDED_COM_CLASSIFIERIMP
#define INCLUDED_COM_CLASSIFIERIMP


#include <cstring>
#include <vector>



namespace com {



/*!
  \class ClassifierImp
  \brief The ClassifierImp class is an abstract base class for concrete
         classes containing classification algorithms.

  This class defines the interface for sub classes which implement an algorithm
  for classifying a continuous range in data values.

  \todo Let the size of vector which enters the classify members determine the
        number of classes and lose the n argument.
*/
template<class T>
class ClassifierImp
{

private:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   ClassifierImp       ();

  //! Destructor.
  virtual          ~ClassifierImp      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual void     classify            (std::vector<T> &b,
                                        T               min,
                                        T               max,
                                        size_t          n) = 0;

  virtual void     autoClassify        (std::vector<T> &b,
                                        T               min,
                                        T               max,
                                        size_t          n) = 0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace com

#endif
