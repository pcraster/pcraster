#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_DATE_TIME_POSIX_TIME_PTIME
#define INCLUDED_BOOST_DATE_TIME_POSIX_TIME_PTIME
#include "boost/date_time/posix_time/ptime.hpp"
#endif

namespace pcrxsd {

  template<typename T>
     struct RangeSetTypeTrait;

  template<>
    struct RangeSetTypeTrait<float> {
        typedef pcrxml::FloatRangeOrSet RangeOrSet;
        typedef pcrxml::FloatRange      Range;
        typedef pcrxml::FloatSet        Set;
    };

  template<>
    struct RangeSetTypeTrait<size_t> {
        typedef pcrxml::OneBasedIntegerRangeOrSet RangeOrSet;
        typedef pcrxml::OneBasedIntegerRange      Range;
        typedef pcrxml::OneBasedIntegerSet        Set;
    };
  template<>
    struct RangeSetTypeTrait<std::string> {
        typedef pcrxml::StringSet                 Set;
    };

  //! item contents of set to vector
  template<typename T>
    std::vector<T> items(
            typename pcrxsd::RangeSetTypeTrait<T>::Set const& s);

  //! container of item contents of set to single vector (flatten)
  template<
    typename ItemType,
     typename Container>
   std::vector<ItemType> flattenItems(Container const& c);

  boost::posix_time::ptime toPosixTime(
    xml_schema::date_time const& dateTime);
  boost::posix_time::time_duration toPosixTimeDuration(
    pcrxml::TimeDuration const& duration);


  // INLINE FUNCTIONS

  template<typename T>
    inline  std::vector<T> items(
            typename pcrxsd::RangeSetTypeTrait<T>::Set const& s)
    {
        std::vector<T> r;
        for(typename pcrxsd::RangeSetTypeTrait<T>::Set::item_const_iterator i=s.item().begin();
                    i!=s.item().end();++i)
                r.push_back(*i);
            return r;
    }

  template<
    typename ItemType,
     typename Container>
     std::vector<ItemType> flattenItems(Container const& c) {
      std::vector<std::string> allItems;
      for(size_t i=0; i < c.size(); ++i) {
       std::vector<ItemType> items(pcrxsd::items<ItemType>(c[i]));
       allItems.insert(allItems.end(),items.begin(),items.end());
      }
      return allItems;
    }


}
