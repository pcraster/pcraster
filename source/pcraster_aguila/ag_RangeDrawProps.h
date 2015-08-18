#ifndef INCLUDED_AG_RANGEDRAWPROPS
#define INCLUDED_AG_RANGEDRAWPROPS



#include <boost/tuple/tuple.hpp>
#include "com_classifier.h"
#include "ag_DrawProps.h"
#include "ag_Types.h"



namespace com {
  class RawPalette;
}



namespace ag {



//! The RangeDrawProps class holds properties for drawing ranges of values.
/*!
  A range of values is a continuous range of values, eg 1.0 - 100.0. To
  visualize such a range in a map or a graph it needs to be classified. Also,
  colours are needed to assign to the classes.
*/
class RangeDrawProps: public DrawProps
{

public:

  typedef boost::tuple<com::Classifier*, com::Classifier*> ClassifierTuple;
  typedef std::vector<ClassifierTuple> ClassifierTuples;

  enum ProbabilityScale {
    CumulativeProbabilities,

    //! P(Z > value).
    ExceedanceProbabilities
  };

private:

  ClassifierTuples _classifiers;

  DrawerType       _drawerType;

  //! Colour used for values lower than min cutoff.
  QColor           _minColour;

  //! Colour used for values higher than max cutoff.
  QColor           _maxColour;

  //! How should the data be presented. Only relevant in case uncertainty information is available.
  ProbabilityScale _probabilityScale;

  void             reMapColours        ();

public:

  typedef com::Classifier::Mode Mode;

  typedef com::Classifier::Algorithm Algorithm;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RangeDrawProps      (const std::string& title,
                                        const com::RawPalette* p,
                                        com::Classifier* rawValueClassifier,
                                  com::Classifier* displayValueClassifier = 0);

                   RangeDrawProps      (RangeDrawProps const& properties);

  /* virtual */    ~RangeDrawProps     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  RangeDrawProps&  operator=           (const RangeDrawProps& rhs);

  bool             equals              (RangeDrawProps const& rhs) const;

  void             setDisplayValueClassifier(com::Classifier* classifier);

  void             unsetDisplayValueClassifier();

  void             setNrClasses        (size_t nrClasses);

  void             setMaxCutoff        (double maxCutoff);

  void             resetMaxCutoff      ();

  void             setMinCutoff        (double cutoff);

  void             resetMinCutoff      ();

  void             setCutoffs          (double minCutoff,
                                        double maxCutoff);

  void             resetCutoffs        ();

  void             setMode             (Mode mode);

  void             setProbabilityScale (ProbabilityScale scale);

  void             setAlgorithm        (Algorithm algorithm);

  void             classify            ();

  void             setDrawerType       (DrawerType type);

  void             assignLabels        ();

  void             merge               (RangeDrawProps const& properties);

  void             pushClassifier      (com::Classifier* rawValueClassifier,
                                        com::Classifier* displayValueClassifier=0);

  ClassifierTuple  popClassifier       ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  ClassifierTuples const& classifiers  () const;

  ClassifierTuples& classifiers        ();

  size_t           rawClassIndex       (double value) const;

  const std::vector<double>& rawClassBorders() const;

  std::vector<double> classBorders     () const;

  double           rawToDisplay        (double value) const;

  com::Classifier* displayValueClassifier();

  const com::Classifier* displayValueClassifier() const;

  com::Classifier* rawValueClassifier  ();

  const com::Classifier* rawValueClassifier() const;

  Mode             mode                () const;

  ProbabilityScale probabilityScale    () const;

  Algorithm        algorithm           () const;

  bool             cutoffsAreValid     () const;

  double           min                 () const;

  double           max                 () const;

  double           minCutoff           () const;

  double           maxCutoff           () const;

  DrawerType       drawerType          () const;

  size_t           nrClassesRequested  () const;

  std::string      label               (REAL4 const& value) const;

  QColor const&    minColour           () const;

  QColor const&    maxColour           () const;

  QColor const&    colour              (double value) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (RangeDrawProps const& lhs,
                                        RangeDrawProps const& rhs);

bool               operator!=          (RangeDrawProps const& lhs,
                                        RangeDrawProps const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
