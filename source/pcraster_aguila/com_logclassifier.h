#ifndef INCLUDED_COM_LOGCLASSIFIER
#define INCLUDED_COM_LOGCLASSIFIER



#include <vector>
#include "com_classifierimp.h"



// namespace com {



/*!
  \class com_LogClassifier
  \brief The com_LogClassifier class if for algorithm objects for classifying
         a continuous range of data values.

  This class implements an classification algorithm. A continuous range of data
  values is devided into classes using the std::log10() function.
*/
//       1         2         3         4         5         6         7         8
template<class T>
class com_LogClassifier: public com::ClassifierImp<T>
{

private:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   com_LogClassifier   ();

  //! Destructor.
                   ~com_LogClassifier  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             classify            (std::vector<T> &b,
                                        T               min,
                                        T               max,
                                        size_t          n);

  void             autoClassify        (std::vector<T> &b,
                                        T               min,
                                        T               max,
                                        size_t               n);

  //----------------------------------------------------------------------------
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



// } // namespace pack

#endif
