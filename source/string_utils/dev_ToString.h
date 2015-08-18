#ifndef INCLUDED_DEV_TOSTRING
#define INCLUDED_DEV_TOSTRING

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// Project headers.

// Module headers.



namespace dev {

//! Convert type T to a string.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Test.
*/
template<typename T>
std::string toString(
         T const& value)
{
  std::ostringstream stream;

  stream << value;

  assert(stream);

  return stream.str();
}



//! Puts the elements pointed to by \a begin and \a end in a string.
/*!
  \param     begin Pointer to first element.
  \param     end Pointer to one past the last element.
  \param     String which will be put between the values.
  \return    string.
  \warning   An empty string is returned if \a begin == \a end.
  \todo      Test.
*/
template<typename T>
std::string toString(
         T* begin,
         T* end,
         std::string const& separator)
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
  \todo      Test.
*/
template<typename T>
std::string toString(
         typename std::vector<T>::const_iterator begin,
         typename std::vector<T>::const_iterator end,
         std::string const& separator)
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
  \param     printer Functor to convert value to a string.
  \param     String which will be put between the values.
  \return    string.
  \warning   An empty string is returned if \a begin == \a end.
  \todo      Test.
*/
template<typename T, class Printer>
std::string toString(
         typename std::vector<T>::const_iterator begin,
         typename std::vector<T>::const_iterator end,
         Printer printer,
         std::string const& separator)
{
  std::ostringstream stream;

  if(begin != end) {

    for(;begin != end - 1; ++begin) {
      stream << printer(*begin) << separator;
    }

    stream << printer(*(begin));
  }

  return stream.str();
}

#endif

} // namespace dev
