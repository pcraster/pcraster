#ifndef INCLUDED_DAL_BASICTYPES
#define INCLUDED_DAL_BASICTYPES



// Library headers.
#ifndef INCLUDED_BOOST_TYPE_TRAITS
#include <boost/type_traits.hpp>
#define INCLUDED_BOOST_TYPE_TRAITS
#endif

#if defined(_MSC_VER)
  #if _MSC_VER
    #pragma warning(disable: 4244)
  #endif
#endif
#ifndef INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC
#include <boost/spirit/include/classic.hpp>
#define INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC
#endif
#if defined(_MSC_VER)
  #if _MSC_VER
    #pragma warning(default: 4244)
  #endif
#endif

#ifndef INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC_STORED_RULE
#include <boost/spirit/include/classic_stored_rule.hpp>
#define INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC_STORED_RULE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_TYPE
#include "dal_Type.h"
#define INCLUDED_DAL_TYPE
#endif


namespace dal {
  // BasicTypes declarations.
}



namespace dal {

template<
  typename T>
class BasicType : public Type {
   public:
     size_t size() const {
       return sizeof(T);
     }
     TypeId id() const {
       return TypeTraits<T>::typeId;
     }
     bool hasTrivialCopy() const {
            return boost::has_trivial_copy<T>();
     }
};

class Uint1Type: public BasicType<UINT1>
{

public:

  typedef boost::spirit::classic::uint_parser<UINT1, 10, 1, 3> Parser;

private:

  Parser           d_parser;

public:

                   Uint1Type           ();

  bool             canParse            (std::string const& string) const;

};



class Uint2Type: public BasicType<UINT2>
{

public:

  typedef boost::spirit::classic::uint_parser<UINT2, 10, 1, 5> Parser;

private:

  Parser           d_parser;

public:

                   Uint2Type           ();

  bool             canParse            (std::string const& string) const;

};



class Uint4Type: public BasicType<UINT4>
{

public:

  typedef boost::spirit::classic::uint_parser<UINT4, 10, 1, 10> Parser;

private:

  Parser           d_parser;

public:

                   Uint4Type           ();

  bool             canParse            (std::string const& string) const;

};



class Int1Type: public BasicType<INT1>
{

public:

  typedef boost::spirit::classic::int_parser<INT1> Parser;

private:

  Parser           d_parser;

public:

                   Int1Type            ();

  bool             canParse            (std::string const& string) const;

};



class Int2Type: public BasicType<INT2>
{

public:

  typedef boost::spirit::classic::int_parser<INT2> Parser;

private:

  Parser           d_parser;

public:

                   Int2Type            ();

  bool             canParse            (std::string const& string) const;

};



class Int4Type: public BasicType<INT4>
{

public:

  typedef boost::spirit::classic::int_parser<INT4> Parser;

private:

  Parser           d_parser;

public:

                   Int4Type            ();

  bool             canParse            (std::string const& string) const;

};



class Real4Type: public BasicType<REAL4>
{

public:

  typedef boost::spirit::classic::real_parser<REAL4, boost::spirit::classic::real_parser_policies<REAL4> > Parser;

private:

  Parser           d_parser;

public:

                   Real4Type           ();

  bool             canParse            (std::string const& string) const;

};



class Real8Type: public BasicType<REAL8>
{

public:

  typedef boost::spirit::classic::real_parser<REAL8, boost::spirit::classic::real_parser_policies<REAL8> > Parser;

private:

  Parser           d_parser;

public:

                   Real8Type           ();

  bool             canParse            (std::string const& string) const;

};


class StringType: public BasicType<std::string>
{

public:

private:

public:

                   StringType          ();

  bool             canParse            (std::string const& string) const;

};


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
