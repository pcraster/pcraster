#ifndef INCLUDED_AG_AGUILAGUITEST
#define INCLUDED_AG_AGUILAGUITEST



// External headers.
#include <QtTest/QtTest>

// Project headers.

// Module headers.



namespace ag {
  // AguilaGuiTest declarations.
}



namespace ag {

//! This class implements the unit tests for the AguilaGui class.
class AguilaGuiTest: public QObject
{

private:

  Q_OBJECT

  size_t const     d_nrVisualisationsPerCursorDialog;

  size_t const     d_nrVisualisationsPerMapWindow;

  size_t const     d_nrVisualisationsPerTimePlotWindow;

  size_t const     d_nrVisualisationsPerAnimationDialog;

  void             testNotExisting     (std::string const& name);

private Q_SLOTS:

  void             initTestCase        ();

  void             cleanupTestCase     ();

  void             init                ();

  void             cleanup             ();

  void             testNotExisting     ();

  void             testRasterMinMaxEqual();

  void             testDataset1        ();

  void             testMultipleViews   ();

public:

                   AguilaGuiTest       ();

};

} // namespace ag

#endif
