#include "com_legendclass.h"
#include <string>



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

