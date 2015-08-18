#ifndef INCLUDED_COM_CLASSIFIER
#define INCLUDED_COM_CLASSIFIER

#include <vector>
#include "csftypes.h"
#include "com_classifierimp.h"
#include "com_linclassifier.h"
#include "com_logclassifier.h"
#include "com_tlogclassifier.h"


namespace com {
template<typename T>
  class UserDefinedClassifier;
}



namespace com {



/*!
  \class Classifier
  \brief The Classifier class is for classifying class borders.
  \sa    com_LinClassifier, com_LogClassifier, com_TLogClassifier

  Classifier objects know how to classify a continuous range of data values
  using the current classification algorithm (see installLin(), installLog()
  and installTLog()).

  After creation some classification parameters can be set explicitly using
  setExtremes(), setMinCutoff(), setMaxCutoff(), setCutoffs() and
  setNrClasses().

  Classborders are calculated in the classify() and classify(size_t) member
  functions. All classification parameters must be set prior to calculating
  class borders.
*/
class Classifier
{

public:

  //! Possible classification types.
  enum Algorithm { LIN, LOG, TLOG, USERDEFINED, INVALID_ALGORITHM };

  //! Possible classification modes.
  enum Mode { INVALID_MODE, AUTO, EXACT };

private:

  //! Pointer to the algorithm used to classify the data.
  com::ClassifierImp<REAL8> *d_classifier;

  //! Current classification type pointed to by d_classifier.
  Algorithm        d_algorithm;

  //! Current classification mode.
  Mode             d_mode;

  //! Classborders, if calculated by calling classify() or classify(size_t).
  std::vector<REAL8> d_borders;

  //! Minimum data value.
  REAL8            d_min;

  //! Maximum data value.
  REAL8            d_max;

  //! Minimum cutoff value.
  REAL8            d_minCutoff;

  //! Maximum cutoff value.
  REAL8            d_maxCutoff;

  //! Requested number of classes.
  size_t           d_nrClasses;

  //! Initialises member variables.
  void             init                ();

  //! Frees dynamically allocated memory.
  void             clean               ();

public:

  //! Type of pointer to a class border.
  typedef std::vector<REAL8>::const_iterator const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
                   Classifier          ();

  //! Copy constructor.
                   Classifier          (const Classifier &rhs);

  //! Constructor.
                   Classifier          (REAL8 min,
                                        REAL8 max);

  //! Destructor.
                   ~Classifier         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  Classifier &     operator=           (const Classifier &rhs);

  bool             equals              (Classifier const& rhs) const;

  //! Install algorithm type \a a.
  void             installAlgorithm    (Algorithm a);

  //! Installs and returns the linear classification algorithm.
  com_LinClassifier<REAL8> *installLin ();

  //! Installs and returns the logarithmic classification algorithm.
  com_LogClassifier<REAL8> *installLog ();

  //! Installs and returns the translated logarithmic classification algorithm.
  com_TLogClassifier<REAL8> *installTLog();

  UserDefinedClassifier<REAL8> *installUserDefined();

  //! Classify the data. Calculate class borders. Use current settings.
  void             classify            ();

  //! Classify the data. Calculate class borders. Use current settings.
  void             classify            (size_t n);

  //! Sets the extremes to \a min and \a max.
  void             setExtremes         (REAL8 min,
                                        REAL8 max);

  //! Sets the cutoff values to \a min and \a max.
  void             setCutoffs          (REAL8 min,
                                        REAL8 max);

  //! Sets the minimum cutoff value to \a v.
  void             setMinCutoff        (REAL8 v);

  //! Sets the maximum cutoff value to \a v.
  void             setMaxCutoff        (REAL8 v);

  //! Resets the cutoff values to the real min and max values.
  void             resetCutoffs        ();

  void             resetMinCutoff      ();

  void             resetMaxCutoff      ();

  //! Sets the requested number of classes to \a n.
  void             setNrClasses        (size_t n);

  //! Sets the classification mode to \a m.
  void             setMode             (Mode m);

  void             merge               (Classifier const& classifier);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the number of classes calculated (== nrBorders() - 1).
  size_t           nrClasses           () const;

  //! Returns the number of classborders calculated.
  size_t           nrBorders           () const;

  //! Returns the number of classes requested.
  size_t           nrClassesRequested  () const;

  //! Returns the class borders.
  const std::vector<REAL8> &borders    () const;

  //! Returns the class index of value \a v.
  size_t           classIndex          (REAL8 v) const;

  //! Returns the upper class border of class with index \a i.
  REAL8            classBorder         (size_t i) const;

  //! Returns an iterator to the first class border, or end().
  const_iterator   begin               () const;

  //! Returns an iterator to the one-past-the-last class border.
  const_iterator   end                 () const;

  //! Returns the algorithm currently installed.
  Algorithm        algorithm           () const;

  //! Returns the currently set minimum data value.
  REAL8            min                 () const;

  //! Returns the currently set maximum data value.
  REAL8            max                 () const;

  bool             extremesAreValid    () const;

  bool             cutoffsAreValid     () const;

  //! Returns the currently set minimum cutoff value.
  REAL8            minCutoff           () const;

  //! Returns the currently set maximum cutoff value.
  REAL8            maxCutoff           () const;

  //! Returns the current classification mode.
  Mode             mode                () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (Classifier const& lhs,
                                        Classifier const& rhs);

bool               operator!=          (Classifier const& lhs,
                                        Classifier const& rhs);

} // namespace com

#endif
