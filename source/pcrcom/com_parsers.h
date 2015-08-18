#ifndef INCLUDED_COM_PARSERS
#define INCLUDED_COM_PARSERS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
// #define BOOST_SPIRIT_DEBUG
#define BOOST_SPIRIT_USE_OLD_NAMESPACE
#ifndef INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC_CORE
#include <boost/spirit/include/classic_core.hpp>
#define INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC_CORE
#endif
#ifndef INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC_UTILITY
#include <boost/spirit/include/classic_utility.hpp>
#define INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC_UTILITY
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  // Parsers declarations.
}



namespace com {

struct CommentGrammar: public boost::spirit::grammar<CommentGrammar>
{
  template<typename ScannerT> struct definition {

    boost::spirit::rule<ScannerT> comment;

    definition(CommentGrammar const& ) {
      comment = "#" >> *boost::spirit::anychar_p;
      // boost::spirit::comment_p("#");
    }

    boost::spirit::rule<ScannerT> const& start() const {
      return comment;
    }
  };
};

struct SectionHeaderGrammar: public boost::spirit::grammar<SectionHeaderGrammar>
{
  template<typename ScannerT> struct definition {

    boost::spirit::rule<ScannerT> rule;

    definition(SectionHeaderGrammar const& ) {
      rule =
        boost::spirit::str_p("binding") |
        boost::spirit::str_p("areamap") |
        boost::spirit::str_p("timer"  ) |
        boost::spirit::str_p("initial") |
        boost::spirit::str_p("dynamic");
    }

    boost::spirit::rule<ScannerT> const& start() const {
      return rule;
    }
  };
};

struct CommandLineArgumentGrammar: public boost::spirit::grammar<CommandLineArgumentGrammar>
{
  template<typename ScannerT> struct definition {

    boost::spirit::rule<ScannerT> rule;

    definition(CommandLineArgumentGrammar const& ) {
      rule = '$' >> boost::spirit::digit_p;
    }

    boost::spirit::rule<ScannerT> const& start() const {
      return rule;
    }
  };
};

struct EnvironmentVariableGrammar: public boost::spirit::grammar<EnvironmentVariableGrammar>
{
  template<typename ScannerT> struct definition {

    boost::spirit::rule<ScannerT> rule;

    definition(EnvironmentVariableGrammar const& ) {
      rule = '$' >> +boost::spirit::alpha_p;
    }

    boost::spirit::rule<ScannerT> const& start() const {
      return rule;
    }
  };
};

struct VariableNameGrammar: public boost::spirit::grammar<VariableNameGrammar>
{
  template<typename ScannerT> struct definition {

    boost::spirit::rule<ScannerT> rule;

    definition(VariableNameGrammar const& ) {
      rule = boost::spirit::alpha_p >>
         *(boost::spirit::alpha_p | boost::spirit::alnum_p);
    }

    boost::spirit::rule<ScannerT> const& start() const {
      return rule;
    }
  };
};

//! name as [a-zA-Z]+
struct PureAlphabeticNameGrammar:
  public boost::spirit::grammar<PureAlphabeticNameGrammar>
{
  template<typename ScannerT> struct definition {

    boost::spirit::rule<ScannerT> rule;

    definition(PureAlphabeticNameGrammar const& ) {
      rule = +(boost::spirit::alpha_p);
    }

    boost::spirit::rule<ScannerT> const& start() const {
      return rule;
    }
  };
};

struct NumberGrammar: public boost::spirit::grammar<NumberGrammar>
{
  template<typename ScannerT> struct definition {

    boost::spirit::rule<ScannerT> rule;

    definition(NumberGrammar const& ) {
      rule = boost::spirit::real_p;
    }

    boost::spirit::rule<ScannerT> const& start() const {
      return rule;
    }
  };
};

struct FileNameGrammar: public boost::spirit::grammar<FileNameGrammar>
{
  template<typename ScannerT> struct definition {

    boost::spirit::rule<ScannerT> rule;

    definition(FileNameGrammar const& ) {
      // 8.3 format
      rule = boost::spirit::alpha_p >>
             boost::spirit::repeat_p(0, 7)[boost::spirit::alnum_p] >>
             !(
               '.' >>
               boost::spirit::repeat_p(0, 3)[boost::spirit::alnum_p]
              );

      /*
          boost::spirit::graph_p >>
         !boost::spirit::graph_p >>
         !boost::spirit::graph_p >>
         !boost::spirit::graph_p >>
         !boost::spirit::graph_p >>
         !boost::spirit::graph_p >>
         !boost::spirit::graph_p >>
         !boost::spirit::graph_p >>
         !(boost::spirit::ch_p('.') >>
           !boost::spirit::graph_p >>
           !boost::spirit::graph_p >>
           !boost::spirit::graph_p
          );
          */
    }

    boost::spirit::rule<ScannerT> const& start() const {
      return rule;
    }
  };
};

/*
struct expressionGrammar: public boost::spirit::grammar<expressionGrammar>
{
  template<typename ScannerT> struct definition {

    boost::spirit::rule<ScannerT> rule;

    definition(FileNameGrammar const& ) {
      // 8.3 format
      rule = boost::spirit::graph_p >> !boost::spirit::graph_p >>
         !boost::spirit::graph_p >> !boost::spirit::graph_p >>
         !boost::spirit::graph_p >> !boost::spirit::graph_p >>
         !boost::spirit::graph_p >> !boost::spirit::graph_p >>
         !(boost::spirit::ch_p('.') >>
         !boost::spirit::graph_p >> !boost::spirit::graph_p >>
         !boost::spirit::graph_p);
    }

    boost::spirit::rule<ScannerT> const& start() const {
      return rule;
    }
  };
};
*/

} // namespace com

#endif
