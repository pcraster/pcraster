#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_LEGEND
#include "com_legend.h"
#define INCLUDED_COM_LEGEND
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_COM_LEGENDCLASS
#include "com_legendclass.h"
#define INCLUDED_COM_LEGENDCLASS
#endif



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

template<class T>
com::Legend<T>::Legend()
{
}



template<class T>
com::Legend<T>::Legend(const std::string &t)

  : d_title(t)

{
}



template<class T>
com::Legend<T>::Legend(const std::vector<com_LegendClass<T> > &c)

  : d_classes(c)

{
}



template<class T>
com::Legend<T>::Legend(const std::string &t,
                          const std::vector<com_LegendClass<T> > &c)

  : d_title(t), d_classes(c)

{
}



template<class T>
com::Legend<T>::~Legend()
{
}



template<class T>
void com::Legend<T>::setTitle(const std::string &t)
{
  d_title = t;
}



template<class T>
void com::Legend<T>::setClasses(const std::vector<com_LegendClass<T> > &c)
{
  d_classes = c;
}



template<class T>
const std::string &com::Legend<T>::title() const
{
  return d_title;
}



template<class T>
const std::vector<com_LegendClass<T> > &com::Legend<T>::classes() const
{
  return d_classes;
}



template<class T>
typename com::Legend<T>::iterator com::Legend<T>::begin()
{
  return d_classes.begin();
}



template<class T>
typename com::Legend<T>::iterator com::Legend<T>::end()
{
  return d_classes.end();
}



template<class T>
typename com::Legend<T>::const_iterator com::Legend<T>::begin() const
{
  return d_classes.begin();
}



template<class T>
typename com::Legend<T>::const_iterator com::Legend<T>::end() const
{
  return d_classes.end();
}



template<class T>
size_t com::Legend<T>::nrClasses() const
{
  return d_classes.size();
}



template<class T>
void com::Legend<T>::setNrClasses(size_t n)
{
  d_classes.resize(n);
}



template<class T>
bool com::Legend<T>::binary_search(T c) const
{
//   // Doesn't work in msvs-8.0
//   return std::binary_search(d_classes.begin(), d_classes.end(), c,
//                             ::compClass<T>());

                            // ::bind2nd(equalClass<T>(), c));


  size_t i = 0;

  while(i < nrClasses() && d_classes[i].value() != c) {
    i++;
  }

  return i < d_classes.size();
}



template<class T>
typename com::Legend<T>::iterator com::Legend<T>::lower_bound(T c)
{
//   // Doesn't work in msvs-8.0
//   return std::lower_bound(d_classes.begin(), d_classes.end(), c, 
//                           ::compClass<T>());

  size_t i = 0;

  while(i < nrClasses() && d_classes[i].value() != c)
    i++;

#ifdef DEBUG_DEVELOP
  POSTCOND(d_classes[i].value() == c);
#endif

  return d_classes.begin() + i;
}



template<class T>
void com::Legend<T>::insert(iterator p, T c)
{
  d_classes.insert(p, com_LegendClass<T>(c));
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


