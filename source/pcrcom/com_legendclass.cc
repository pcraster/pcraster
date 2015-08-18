#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_LEGENDCLASS
#include "com_legendclass.h"
#define INCLUDED_COM_LEGENDCLASS
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



template<class T>
com_LegendClass<T>::com_LegendClass()
{
}



template<class T>
com_LegendClass<T>::com_LegendClass(T v)

  : d_value(v)

{
}



template<class T>
com_LegendClass<T>::com_LegendClass(T v, const std::string &d)

  : d_value(v), d_descr(d)

{
}



template<class T>
com_LegendClass<T>::~com_LegendClass()
{
}



template<class T>
void com_LegendClass<T>::setValue(T v)
{
  d_value = v;
}



template<class T>
void com_LegendClass<T>::setDescr(const std::string &d)
{
  d_descr = d;
}



template<class T>
T com_LegendClass<T>::value() const
{
  return d_value;
}



template<class T>
const std::string &com_LegendClass<T>::descr() const
{
  return d_descr;
}

