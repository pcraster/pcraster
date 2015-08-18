#ifndef INCLUDED_COM_STRCONV
#define INCLUDED_COM_STRCONV

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif


namespace com {
  //! convert from string to numeric type
  /*! \a s may contain leading and trailing whitespaces, which
   *  conform to most libraries, and the vice versa process
   *  number to string may yield those space due to formatting.
   *  Only decimal notation is supported.
   *
   *  \throws
   *    std::range_error if not a valid number, what() has
   *    messages like: not a "type description"
   *
   *  Code is taken from boost::lexical_cast but still has
   *  flaws and crashed for under strange conditions under gcc.
   *  see fromStringNotWorking for failed implementation. The trouble
   *  seems  to be in gcc (last check on gcc prior to 3.0).
   *
   *  Therefor only the specializations for
   *  int,double, size_t are implemented. If one needs another
   *  one write the tests!
   *
   */
  template<typename T>
   inline T fromString(const std::string& s);

  /*!
   *  \todo
   *    try again gcc 3.0 or newer
   */
  template<typename T>
  T fromStringNotWorking(const std::string& s)
  {
    PRECOND(FALSE);
    PRECOND(s.empty());
    T    value;
#ifdef NEVER
    std::cout << s << " to convert\n";
#ifdef __GNUC__
    // gcc 2.96 crashes if using stringstream
    std::strstream interpreter;
#else
    std::stringstream interpreter;
#endif


    if(!(interpreter << s))
        throw std::range_error("not a number");
    if (!(interpreter >> value))
        throw std::range_error("not a number");
    /* it is said that due to a defect in the standard
     * this conversion can not go direct to the desired
     * type. I have seen that -1 for size_t goes to 2^32-1
     * which is not good
     *
     * long l;
     * if (!(interpreter >> l))
     *  throw std::range_error("not a number");
     * if (l < com::NumericLimits<T>::minValue() || 
     *     l > com::NumericLimits<T>::maxValue())
     *  // need C-style cast
     * value = (T)l;
     * BUT, gcc 2.96 does not have numeric limits
     */
    // If stuff is still available, it should be white space characters.
    if (!(interpreter >> std::ws).eof())
         throw std::range_error("not a number");
#endif
    return value;
  }

  //! see T fromString(const std::string& s);
  template<>
  inline size_t fromString<size_t>(const std::string& s) {
    return strToSize_t(s);
  }
  //! see T fromString(const std::string& s);
  template<>
   inline int fromString<int>(const std::string& s) {
    return strToInt(s);
  }
  //! see T fromString(const std::string& s);
  template<>
  inline double fromString<double>(const std::string& s) {
    return strToDouble(s);
  }

  //! convert type T to a string
  /*!
      \sa size_tToStr intToStr longToStr doubleToStr
   */
  template<typename T>
  std::string toString(const T &value)
  {
    std::ostringstream stream;
    stream << value;
    POSTCOND(stream);
    return stream.str();
  }

//! Puts the elements pointed to by \a begin and \a end in a string.
/*!
  \param     begin Pointer to first element.
  \param     end Pointer to one past the last element.
  \param     String which will be put between the values.
  \return    string.
  \warning   An empty string is returned if \a begin == \a end.
*/
template<typename T>
std::string toString(T* begin, T* end, const std::string& separator)
{
  std::ostringstream stream;

  if(begin != end) {

    for(typename std::vector<T>::const_iterator it = begin; it != end - 1;
         ++it) {
      stream << toString(*it) << separator;
    }

    stream << toString(*(end - 1));
  }

  return stream.str();
}



//! Puts the elements pointed to by \a begin and \a end in a string.
/*!
  \param     begin Pointer to first element.
  \param     end Pointer to one past the last element.
  \param     String which will be put between the values.
  \return    string.
  \warning   An empty string is returned if \a begin == \a end.
*/
template<typename T>
std::string toString(typename std::vector<T>::const_iterator begin,
                   typename std::vector<T>::const_iterator end,
                   const std::string& separator)
{
  std::ostringstream stream;

  if(begin != end) {

    for(;begin != end - 1; ++begin) {
      stream << toString(*begin) << separator;
    }

    stream << toString(*(begin));
  }

  return stream.str();
}



//! Puts the elements pointed to by \a begin and \a end in a string.
/*!
  \param     begin Pointer to first element.
  \param     end Pointer to one past the last element.
  \param     Function to convert value to a string.
  \param     String which will be put between the values.
  \return    string.
  \warning   An empty string is returned if \a begin == \a end.
*/
template<typename T>
std::string toString(typename std::vector<T>::const_iterator begin,
                   typename std::vector<T>::const_iterator end,
                   std::string (*toString)(const T&),
                   const std::string& separator)
{
  std::ostringstream stream;

  if(begin != end) {

    for(;begin != end - 1; ++begin) {
      stream << toString(*begin) << separator;
    }

    stream << toString(*(begin));
  }

  return stream.str();
}



//! Puts the elements pointed to by \a begin and \a end in a string.
/*!
  \param     begin Pointer to first element.
  \param     end Pointer to one past the last element.
  \param     Function to convert value to a string.
  \param     String which will be put between the values.
  \return    string.
  \warning   An empty string is returned if \a begin == \a end.
*/
template<typename T>
std::string toString(typename std::vector<T>::const_iterator begin,
                   typename std::vector<T>::const_iterator end,
                   std::string (*toString)(T),
                   const std::string& separator)
{
  std::ostringstream stream;

  if(begin != end) {

    for(;begin != end - 1; ++begin) {
      stream << toString(*begin) << separator;
    }

    stream << toString(*(begin));
  }

  return stream.str();
}

}

#endif
