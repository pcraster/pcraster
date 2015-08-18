#ifndef INCLUDED_DEV_UTILS
#define INCLUDED_DEV_UTILS



// External headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
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

//! Removes duplicate elements from the \a collection while preserving the order.
/*!
  \tparam    T Type of the elements in the collection.
  \param     collection Collection to update.
*/
template<class T>
void unique(
         std::vector<T>& collection)
{
  if(!collection.empty()) {
    typename std::vector<T>::iterator end(collection.end());
    typename std::vector<T>::iterator begin(collection.begin());

    // Position of current element being considered.
    typename std::vector<T>::const_iterator pos = begin;

    while(++begin != end) {
      end = std::remove(begin, end, *pos);
      pos = begin;
    }

    collection.erase(end, collection.end());
  }
}



void               end                 (std::ostringstream& stream);

bool               environmentVariableSet(
                                        std::string const& name);

std::string        environmentVariable (std::string const& name);

void               setEnvironmentVariable(
                                        std::string const& name,
                                        std::string const& value);

void               unsetEnvironmentVariable(
                                        std::string const& name);

}

#endif
