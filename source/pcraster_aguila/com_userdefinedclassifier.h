#ifndef INCLUDED_COM_USERDEFINEDCLASSIFIER
#define INCLUDED_COM_USERDEFINEDCLASSIFIER



// Library headers.

// PCRaster library headers.

// Module headers.
#include "com_classifierimp.h"



namespace com {
  // UserDefinedClassifier declarations.
}



namespace com {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
template<typename T>
class UserDefinedClassifier: public ClassifierImp<T>
{

  friend class UserDefinedClassifierTest;

private:

  std::vector<T>   d_borders;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   UserDefinedClassifier();

  /* virtual */    ~UserDefinedClassifier();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setBorders          (std::vector<T> const& borders);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             classify            (std::vector<T> &borders,
                                        T min,
                                        T max,
                                        size_t numberOfClasses);

  void             autoClassify        (std::vector<T> &borders,
                                        T min,
                                        T max,
                                        size_t numberOfClasses);

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
