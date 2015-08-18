#ifndef INCLUDED_COM_LINCLASSIFIER
#define INCLUDED_COM_LINCLASSIFIER



#include <vector>
#include "com_classifierimp.h"



// namespace com {



/*!
  \class com_LinClassifier
  \brief The com_LinClassifier class is for algorithm objects for classifying
         a continuous range of data values.

  This class implements an classification algorithm. A continuous range of data
  values is devided into classes of equal width.
*/
//       1         2         3         4         5         6         7         8
template<class T>
class com_LinClassifier: public com::ClassifierImp<T>
{

private:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   com_LinClassifier   ();

  //! Destructor.
                   ~com_LinClassifier  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             classify            (std::vector<T> &b,
                                        T               min,
                                        T               max,
                                        size_t               n);

  void             autoClassify        (std::vector<T> &b,
                                        T               min,
                                        T               max,
                                        size_t               n);

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
