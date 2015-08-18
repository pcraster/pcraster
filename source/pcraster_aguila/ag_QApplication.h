#ifndef INCLUDED_AG_QAPPLICATION
#define INCLUDED_AG_QAPPLICATION



// External headers.
#include <QApplication>

// Project headers.

// Module headers.



namespace ag {
  // QApplication declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class QApplication: public ::QApplication
{

  friend class QApplicationTest;

private:

  // Q_OBJECT;

  std::string      _name;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   QApplication        (int& argc,
                                        char** argv);

  /* virtual */    ~QApplication       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  bool             notify              (QObject* receiver,
                                        QEvent* event);

  void             setName             (std::string const& name);

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
