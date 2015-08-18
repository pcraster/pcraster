#include "com_userdefinedclassifier.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the UserDefinedClassifier class.
*/



namespace com {

//------------------------------------------------------------------------------

/*
class UserDefinedClassifierPrivate
{
public:

  UserDefinedClassifierPrivate()
  {
  }

  ~UserDefinedClassifierPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC USERDEFINEDCLASSIFIER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF USERDEFINEDCLASSIFIER MEMBERS
//------------------------------------------------------------------------------

template<typename T>
UserDefinedClassifier<T>::UserDefinedClassifier()
{
}



template<typename T>
UserDefinedClassifier<T>::~UserDefinedClassifier()
{
}



template<typename T>
void UserDefinedClassifier<T>::setBorders(
         std::vector<T> const& borders)
{
  d_borders = borders;
}



template<typename T>
void UserDefinedClassifier<T>::classify(
         std::vector<T> &borders,
         T /* min */,
         T /* max */,
         size_t
#ifdef DEBUG_DEVELOP
         numberOfClasses
#endif
         )
{
  assert(numberOfClasses == borders.size());

  borders = d_borders;
}



template<typename T>
void UserDefinedClassifier<T>::autoClassify(
         std::vector<T> &borders,
         T /* min */,
         T /* max */,
         size_t /* numberOfClasses */)
{
  borders = d_borders;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

