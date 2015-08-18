#include "ag_ScriptSyntaxHighlighter.h"

// Library headers.
#include <iostream>
#include <iterator>

/// #include <boost/spirit.hpp>
/// #include <boost/spirit/core.hpp>
/// #include <boost/spirit/iterator/position_iterator.hpp>
/// #include <boost/spirit/utility.hpp>

// PCRaster library headers.
/// #ifndef INCLUDED_COM_PARSERS
/// #include "com_parsers.h"
/// #define INCLUDED_COM_PARSERS
/// #endif

// Module headers.



/*!
  \file
  This file contains the implementation of the ScriptSyntaxHighlighter class.
*/



namespace ag {

struct IHighlight
{
  QSyntaxHighlighter* d_highlighter;
  QFont            d_font;
  QColor           d_colour;

  IHighlight(QSyntaxHighlighter* highlighter, const QFont& font,
    const QColor& colour)
    : d_highlighter(highlighter), d_font(font), d_colour(colour)
  { }

  template<typename IteratorType>
  void operator()(IteratorType first, IteratorType last) const {
    int start = first.get_position().column - 1;
    int length = std::distance(first, last);
    // Doesn't compile anymore after port to Qt4.
    // d_highlighter->setFormat(start, length, d_font, d_colour);
  }
};

struct HighlightLiteral: public IHighlight
{
  HighlightLiteral(QSyntaxHighlighter* highlighter)
    : IHighlight(highlighter, QFont("Courier", 12), QColor(153, 0, 0))
  { }
};

struct HighlightComment: public IHighlight
{
  HighlightComment(QSyntaxHighlighter* highlighter)
    : IHighlight(highlighter, QFont("Courier", 12), QColor(51, 51, 204))
  { }
};

struct HighlightSectionHeader: public IHighlight
{
  HighlightSectionHeader(QSyntaxHighlighter* highlighter)
    : IHighlight(highlighter, QFont("Courier", 12), QColor(255, 102, 0))
  { }
};

struct HighlightResult: public IHighlight
{
  HighlightResult(QSyntaxHighlighter* highlighter)
    : IHighlight(highlighter, QFont("Courier", 12), Qt::black)
  { }
};

struct HighlightCommandLineArgument: public HighlightLiteral
{
  HighlightCommandLineArgument(QSyntaxHighlighter* highlighter)
    : HighlightLiteral(highlighter)
  { }
};

struct HighlightEnvironmentVariable: public HighlightLiteral
{
  HighlightEnvironmentVariable(QSyntaxHighlighter* highlighter)
    : HighlightLiteral(highlighter)
  { }
};

struct HighlightNumber: public HighlightLiteral
{
  HighlightNumber(QSyntaxHighlighter* highlighter)
    : HighlightLiteral(highlighter)
  { }
};

struct HighlightFileName: public HighlightLiteral
{
  HighlightFileName(QSyntaxHighlighter* highlighter)
    : HighlightLiteral(highlighter)
  { }
};

}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SCRIPTSYNTAXHIGHLIGHTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SCRIPTSYNTAXHIGHLIGHTER MEMBERS
//------------------------------------------------------------------------------

ag::ScriptSyntaxHighlighter::ScriptSyntaxHighlighter(QTextEdit* edit)

  : QSyntaxHighlighter(edit),
    d_fontSize(12)

{
}



ag::ScriptSyntaxHighlighter::~ScriptSyntaxHighlighter()
{
}



int ag::ScriptSyntaxHighlighter::highlightParagraph(const QString& text,
         int /* endStateOfLastPara */)
{
  // Default formatting.
  setFormat(0, text.length(), QFont("Courier", 12) /* , Qt::black */);

//   com::CommentGrammar comment;
//   com::SectionHeaderGrammar sectionHeader;
//   com::VariableNameGrammar result;
//   com::CommandLineArgumentGrammar commandLineArgument;
//   com::EnvironmentVariableGrammar environmentVariable;
//   com::NumberGrammar number;
//   com::FileNameGrammar fileName;
// 
//   std::string line = text.toUtf8().constData();
//   typedef boost::spirit::position_iterator<std::string::iterator> IteratorType;
//   IteratorType begin(line.begin(), line.end(), "");
//   IteratorType end;
// 
//   // Doesn't compile anymore after port to Qt4.
//   // boost::spirit::parse_info<IteratorType> info =
//   //        boost::spirit::parse<IteratorType>(begin, end,
// 
//   // !(
//   //   (
//   //     sectionHeader[HighlightSectionHeader(this)] |
//   //     (result[HighlightResult(this)] >> '=' >>
//   //       (
//   //         commandLineArgument[HighlightCommandLineArgument(this)] |
//   //         environmentVariable[HighlightEnvironmentVariable(this)] |
//   //         number[HighlightNumber(this)] |
//   //         fileName[HighlightFileName(this)]
//   //       )
//   //     ) >> ';'
//   //   )
//   // )
//   // >> !comment[HighlightComment(this)]
//   // ,
// 
//   // boost::spirit::space_p);

  return 0;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
