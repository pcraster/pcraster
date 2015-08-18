#include "com_classifierimp.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

template<class T>
com::ClassifierImp<T>::ClassifierImp()
{
}



template<class T>
com::ClassifierImp<T>::~ClassifierImp()
{
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------

/*!
  \fn      void com::ClassifierImp<T>::classify(std::vector<T> &b, T min, T max, size_t n)
  \brief   Calculates \a n classborders and inserts them into \a b.
  \param   b Datastructure for holding the calculated classborders. It will be
           resized to \a n.
  \param   min Lowest class border.
  \param   max Highest class border.
  \param   n Number of classborders to calculate.
  \return  Classborders through argument \a b.
  \warning The result is undefined if n == 0.
*/

