#ifndef INCLUDED_COM_TLOGCLASSIFIER
#define INCLUDED_COM_TLOGCLASSIFIER



#include <vector>
#include "com_classifierimp.h"



// namespace com {



/*!
  \class com_TLogClassifier
  \brief The com_TLogClassifier class is for algorithm objects for classifying
         a continuous range of data values.

  This class implements a classification algorithm.
*/
//       1         2         3         4         5         6         7         8
template<class T>
class com_TLogClassifier: public com::ClassifierImp<T>
{

private:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   com_TLogClassifier  ();

  //! Destructor.
                   ~com_TLogClassifier ();

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
