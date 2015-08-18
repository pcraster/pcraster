#ifndef INCLUDED_AG_SCRIPTSYNTAXHIGHLIGHTER
#define INCLUDED_AG_SCRIPTSYNTAXHIGHLIGHTER



// Library headers.
#include <QSyntaxHighlighter>

// PCRaster library headers.

// Module headers.



namespace ag {
  // ScriptSyntaxHighlighter declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class ScriptSyntaxHighlighter: public QSyntaxHighlighter
{

private:

  int              d_fontSize;

  //! Assignment operator. NOT IMPLEMENTED.
  ScriptSyntaxHighlighter& operator=   (const ScriptSyntaxHighlighter& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ScriptSyntaxHighlighter(const ScriptSyntaxHighlighter& rhs);

  int              highlightParagraph  (const QString& text,
                                        int endStateOfLastPara);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ScriptSyntaxHighlighter(QTextEdit* edit);

  /* virtual */    ~ScriptSyntaxHighlighter();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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



} // namespace ag

#endif
