#ifndef INCLUDED_DEV_ALGORITHM
#define INCLUDED_DEV_ALGORITHM

// External headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

// Project headers.

// Module headers.



namespace dev {

template<typename Container, typename Element>
bool hasElement(
         Container const& container,
         Element const& element) {
  return std::find(container.begin(),container.end(), element) !=
         container.end();
}



template<class Container, class Operation>
Operation forWhole(
         Container& container,
         Operation operation) {
  return std::for_each(container.begin(), container.end(), operation);
}



//! Function object that deletes the pointer passed to operator().
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Handy to delete container of pointers (see Effective STL item 7).

  \begincode
  com::forWhole(container, com::Delete<T>());
  \endcode
*/
template<class T>
struct Delete: public std::unary_function<const T*, void>
{
  void operator()(T const* item) const {
    delete item;
  }
};


} // namespace dev

#endif
