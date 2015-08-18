#include "com_classifier.h"
#include <algorithm>
#include <cassert>
#include <cmath>
#include <stdexcept>
#include <iostream>
#include <string>
#include <vector>
#include <boost/math/special_functions/fpclassify.hpp>
#include "pcrtypes.h"
#include "com_classifierimp.h"
#include "com_linclassifier.h"
#include "com_logclassifier.h"
#include "com_tlogclassifier.h"
#include "com_userdefinedclassifier.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

/*!
  \warning Doesn't set a default classification algorithm. Set classification
           parameters before calling classify() or classify(size_t)!
  \sa      com::Classifier(const com::Classifier &),
           com::Classifier(REAL8, REAL8)

  The default classification mode is AUTO.
*/
com::Classifier::Classifier()
{
  try
  {
    init();
  }
  catch(...)
  {
    clean();
    throw;
  }
}



/*!
  \param   rhs Object to copy values from.
  \sa      com::Classifier(const com::Classifier &)

  Performs a deep copy of the classification algorithm.
*/
com::Classifier &com::Classifier::operator=(const Classifier &rhs)
{
  if(this != &rhs)
  {
    clean();

    // Set d_classifier and d_algorithm.
    if(rhs.d_algorithm == LIN) {
      com_LinClassifier<REAL8> *c =
                     dynamic_cast<com_LinClassifier<REAL8> *>(rhs.d_classifier);
      assert(c);
      d_classifier = new com_LinClassifier<REAL8>(*c);
      d_algorithm  = LIN;
    }
    else if(rhs.d_algorithm == LOG) {
      com_LogClassifier<REAL8> *c =
                     dynamic_cast<com_LogClassifier<REAL8> *>(rhs.d_classifier);
      assert(c);
      d_classifier = new com_LogClassifier<REAL8>(*c);
      d_algorithm  = LOG;
    }
    else if(rhs.d_algorithm == TLOG) {
      com_TLogClassifier<REAL8> *c =
                    dynamic_cast<com_TLogClassifier<REAL8> *>(rhs.d_classifier);
      assert(c);
      d_classifier = new com_TLogClassifier<REAL8>(*c);
      d_algorithm  = TLOG;
    }
    else if(rhs.d_algorithm == USERDEFINED) {
      UserDefinedClassifier<REAL8> *c =
                   dynamic_cast<UserDefinedClassifier<REAL8> *>(rhs.d_classifier);
      assert(c);
      d_classifier = new UserDefinedClassifier<REAL8>(*c);
      d_algorithm  = USERDEFINED;
    }

    d_mode      = rhs.d_mode;
    d_borders   = rhs.d_borders;

    if(rhs.extremesAreValid()) {
      d_min = rhs.d_min;
      d_max = rhs.d_max;
    }
    else {
      pcr::setMV(d_min);
      pcr::setMV(d_max);
    }

    if(rhs.cutoffsAreValid()) {
      d_minCutoff = rhs.d_minCutoff;
      d_maxCutoff = rhs.d_maxCutoff;
    }
    else {
      pcr::setMV(d_minCutoff);
      pcr::setMV(d_maxCutoff);
    }

    d_nrClasses = rhs.d_nrClasses;
  }

  return *this;
}



/*!
  \param   rhs Object to copy values from.
  \sa      operator=(const com::Classifier &), com::Classifier(),
           com::Classifier(REAL8, REAL8)

  Performs a deep copy of the classification algorithm.
*/
com::Classifier::Classifier(const Classifier &rhs)
{
  init();

  // Set d_classifier and d_algorithm.
  if(rhs.d_algorithm == LIN) {
    com_LinClassifier<REAL8> *c =
         dynamic_cast<com_LinClassifier<REAL8> *>(rhs.d_classifier);
    assert(c);
    d_classifier = new com_LinClassifier<REAL8>(*c);
    d_algorithm  = LIN;
  }
  else if(rhs.d_algorithm == LOG) {
    com_LogClassifier<REAL8> *c =
         dynamic_cast<com_LogClassifier<REAL8> *>(rhs.d_classifier);
    assert(c);
    d_classifier = new com_LogClassifier<REAL8>(*c);
    d_algorithm  = LOG;
  }
  else if(rhs.d_algorithm == TLOG) {
    com_TLogClassifier<REAL8> *c =
         dynamic_cast<com_TLogClassifier<REAL8> *>(rhs.d_classifier);
    assert(c);
    d_classifier = new com_TLogClassifier<REAL8>(*c);
    d_algorithm  = TLOG;
  }
  else if(rhs.d_algorithm == USERDEFINED) {
    UserDefinedClassifier<REAL8> *c =
         dynamic_cast<UserDefinedClassifier<REAL8> *>(rhs.d_classifier);
    assert(c);
    d_classifier = new UserDefinedClassifier<REAL8>(*c);
    d_algorithm  = USERDEFINED;
  }

  d_mode      = rhs.d_mode;
  d_borders   = rhs.d_borders;

  if(rhs.extremesAreValid()) {
    d_min = rhs.d_min;
    d_max = rhs.d_max;
  }
  else {
    pcr::setMV(d_min);
    pcr::setMV(d_max);
  }

  if(rhs.cutoffsAreValid()) {
    d_minCutoff = rhs.d_minCutoff;
    d_maxCutoff = rhs.d_maxCutoff;
  }
  else {
    pcr::setMV(d_minCutoff);
    pcr::setMV(d_maxCutoff);
  }

  d_nrClasses = rhs.d_nrClasses;
}



/*!
  \param   min Minimum data value.
  \param   max Maximum data value.
  \sa      com::Classifier(), com::Classifier(const Classifier &)

  The extremes and the cutoff values are set to \a min and \a max. If \a max
  is smaller than \a min, than they're swapped.
*/
com::Classifier::Classifier(REAL8 min, REAL8 max)
{
  try
  {
    init();
    d_min = min;
    d_max = max;

    if(d_min > d_max)
      std::swap(d_min, d_max);

    d_minCutoff = d_min;
    d_maxCutoff = d_max;
  }
  catch(...)
  {
    clean();
    throw;
  }
}



/*!
  Calls clean().
*/
com::Classifier::~Classifier()
{
  clean();
}



/*!
  After calling this function you can't call the classify() or classify(size_t)
  functions anymore. All members are set to an initial state (empty, invalid,
  0, etc).
*/
void com::Classifier::init()
{
  d_classifier = 0;
  d_borders.erase(d_borders.begin(), d_borders.end());
  d_algorithm  = INVALID_ALGORITHM;
  d_mode       = AUTO;
  pcr::setMV(d_min);
  pcr::setMV(d_max);
  pcr::setMV(d_minCutoff);
  pcr::setMV(d_maxCutoff);
  d_nrClasses  = 0;
}



/*!
  \warning  If you saved a copy of the pointer to that algorithm you're better
            off not using it anymore!

  Deletes the classification algorithm from memory.
*/
void com::Classifier::clean()
{
  delete d_classifier; d_classifier = 0;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  No need to check the classifier, this is determined by d_algorithm.
*/
bool com::Classifier::equals(Classifier const& rhs) const
{
  bool extremesAreEqual = false;

  if(!extremesAreValid()) {
    if(!rhs.extremesAreValid()) {
      // Both objects have invalid/unset extremes.
      extremesAreEqual = true;
    }
  }
  else {
    if(rhs.extremesAreValid()) {
      if(d_min == rhs.d_min && d_max == rhs.d_max) {
        // Both objects have valid and equal extremes.
        extremesAreEqual = true;
      }
    }
  }

  bool cutoffsAreEqual = false;

  if(!cutoffsAreValid()) {
    if(!rhs.cutoffsAreValid()) {
      // Both objects have invalid/unset cutoffs.
      cutoffsAreEqual = true;
    }
  }
  else {
    if(rhs.cutoffsAreValid()) {
      if(d_min == rhs.d_min && d_max == rhs.d_max) {
        // Both objects have valid and equal cutoffs.
        cutoffsAreEqual = true;
      }
    }
  }

  return d_algorithm == rhs.d_algorithm &&
         d_mode == rhs.d_mode &&
         d_borders == rhs.d_borders &&
         extremesAreEqual &&
         cutoffsAreEqual &&
         d_nrClasses == rhs.d_nrClasses;
}



/*!
  \warning Don't forget to set the classification parameters before calling
           this function!
  \sa      setExtremes(), setMinCutoff(), setMaxCutoff(), setCutoffs(),
           setNrClasses()

  The currently set values for minimum cutoff, maximum cutoff and number of
  classes are passed to the classification algorithm.
*/
void com::Classifier::classify()
{
  assert(d_classifier);

  if(!cutoffsAreValid() || d_minCutoff == d_maxCutoff) {
    d_borders.resize(0);
  }
  else {
    assert(d_min <= d_max);
    assert(d_minCutoff <= d_maxCutoff);
    assert(!pcr::isMV(d_min));
    assert(!pcr::isMV(d_max));
    assert(!pcr::isMV(d_minCutoff));
    assert(!pcr::isMV(d_maxCutoff));

    if(d_mode == AUTO) {
      d_classifier->autoClassify(d_borders, d_minCutoff, d_maxCutoff,
         d_nrClasses);
    }
    else if(d_mode == EXACT) {
      d_classifier->classify(d_borders, d_minCutoff, d_maxCutoff, d_nrClasses);
    }
#ifdef DEBUG_DEVELOP
    else {
      throw std::logic_error(std::string("com::Classifier::classify()"));
    }
#endif
  }

  if(!d_borders.empty()) {
    d_minCutoff = d_borders.front();
    d_maxCutoff = d_borders.back();
    d_nrClasses = d_borders.size() - 1;
  }
}



/*!
  \overload
  \param   n Number of class borders to calculate.
*/
void com::Classifier::classify(size_t n)
{
#ifdef DEBUG_DEVELOP
  assert(d_classifier);
#endif

  d_nrClasses = n;
  classify();
}



/*!
  \param   a Algorithm to install.
  \sa      installLin(), installLog(), installTLog()
*/
void com::Classifier::installAlgorithm(Algorithm a)
{
  if(a == LIN)
    (void)installLin();
  else if(a == LOG)
    (void)installLog();
  else if(a == TLOG)
    (void)installTLog();
  else if(a == USERDEFINED)
    (void)installUserDefined();
}



/*!
  \return  Classification object created.
  \warning The pointer returned by this function is for configuring the
           algorithm specific parameters. If the com::Classifier object dies,
           this pointer is not valid anymore and should not be dereferenced!
  \sa      com_LinClassifier, installLog(), installTLog()
*/
com_LinClassifier<REAL8> *com::Classifier::installLin()
{
  delete d_classifier, d_classifier = 0;
  d_borders.erase(d_borders.begin(), d_borders.end());
  d_algorithm  = INVALID_ALGORITHM;

  com_LinClassifier<REAL8> *c = 0;

  try
  {
    c = new com_LinClassifier<REAL8>();
    d_classifier = c;
    d_algorithm = LIN;
  }
  catch(...)
  {
    delete c; c = 0;
    clean();
    throw;
  }

  return c;
}



/*!
  \return  Classification object created.
  \warning The pointer returned by this function is for configuring the
           algorithm specific parameters. If the com::Classifier object dies,
           this pointer is not valid anymore and should not be dereferenced!
  \sa      com_LogClassifier, installLin(), installTLog()
*/
com_LogClassifier<REAL8> *com::Classifier::installLog()
{
  delete d_classifier, d_classifier = 0;
  d_borders.erase(d_borders.begin(), d_borders.end());
  d_algorithm  = INVALID_ALGORITHM;

  com_LogClassifier<REAL8> *c = 0;

  try
  {
    c = new com_LogClassifier<REAL8>();
    d_classifier = c;
    d_algorithm  = LOG;
  }
  catch(...)
  {
    delete c; c = 0;
    clean();
    throw;
  }

  return c;
}



/*!
  \return  Classification object created.
  \warning The pointer returned by this function is for configuring the
           algorithm specific parameters. If the com::Classifier object dies,
           this pointer is not valid anymore and should not be dereferenced!
  \sa      com_TLogClassifier, installLin(), installLog()
*/
com_TLogClassifier<REAL8> *com::Classifier::installTLog()
{
  delete d_classifier, d_classifier = 0;
  d_borders.erase(d_borders.begin(), d_borders.end());
  d_algorithm  = INVALID_ALGORITHM;

  com_TLogClassifier<REAL8> *c = 0;

  try
  {
    c = new com_TLogClassifier<REAL8>();
    d_classifier = c;
    d_algorithm  = TLOG;
  }
  catch(...)
  {
    delete c; c = 0;
    clean();
    throw;
  }

  return c;
}



com::UserDefinedClassifier<REAL8> *com::Classifier::installUserDefined()
{
  delete d_classifier, d_classifier = 0;
  d_borders.erase(d_borders.begin(), d_borders.end());
  d_algorithm  = INVALID_ALGORITHM;

  UserDefinedClassifier<REAL8>* classifier = 0;

  try {
    classifier = new UserDefinedClassifier<REAL8>();
    d_classifier = classifier;
    d_algorithm  = USERDEFINED;
  }
  catch(...) {
    delete classifier; classifier = 0;
    clean();
    throw;
  }

  return classifier;
}



/*!
  \return  The number of classes calculated.
  \warning The value returned by this function is need not have the same value
           as set with the setNrClasses() or classify(size_t) member functions.
           The requested number of classes does not need to be the same as the
           actually calculated number.
  \sa      setNrClasses()

  If classify() or classify(size_t) hasn't been called yet, this function will
  return 0.

  The number of classes is equal to the calculated number of classborders
  minus 1.

  This function returns 0 if no class borders are calculated.
*/
size_t com::Classifier::nrClasses() const
{
  return d_borders.empty() ? 0 : d_borders.size() - 1;
}



size_t com::Classifier::nrBorders() const
{
  return d_borders.size();
}



size_t com::Classifier::nrClassesRequested() const
{
  return d_nrClasses;
}



com::Classifier::const_iterator com::Classifier::begin() const
{
  return d_borders.begin();
}



com::Classifier::const_iterator com::Classifier::end() const
{
  return d_borders.end();
}



com::Classifier::Algorithm com::Classifier::algorithm() const
{
  return d_algorithm;
}



const std::vector<REAL8> &com::Classifier::borders() const
{
  return d_borders;
}



/*!
  \sa      max(), setExtremes()
*/
REAL8 com::Classifier::min() const
{
  assert(!pcr::isMV(d_min));
  return d_min;
}



/*!
  \sa      min(), setExtremes()
*/
REAL8 com::Classifier::max() const
{
  assert(!pcr::isMV(d_max));
  return d_max;
}



/*!
  \sa      maxCutoff(), setCutoffs(), setMinCutoff(), setMaxCutoff()
*/
REAL8 com::Classifier::minCutoff() const
{
  assert(!pcr::isMV(d_minCutoff));
  return d_minCutoff;
}



/*!
  \sa      minCutoff(), setCutoffs(), setMinCutoff(), setMaxCutoff()
*/
REAL8 com::Classifier::maxCutoff() const
{
  assert(!pcr::isMV(d_maxCutoff));
  return d_maxCutoff;
}



/*!
  \param   min New minimum value.
  \param   max New minimum value.
  \sa      min(), max(), setCutoffs(), setMinCutoff(), setMaxCutoff()

  If \a min > \a max they're swapped.
*/
void com::Classifier::setExtremes(REAL8 min, REAL8 max)
{
  d_min = min;
  d_max = max;

  if(d_min > d_max)
    std::swap(d_min, d_max);
}



/*!
  \param   min New minimum cutoff value.
  \param   min New maximum cutoff value.
  \sa      setMinCutoff(), setMaxCutOff(), resetCutoffs(), minCutoff(),
           maxCutoff()

  If \a min > \a max they're swapped.
*/
void com::Classifier::setCutoffs(REAL8 min, REAL8 max)
{
  d_minCutoff = min;
  d_maxCutoff = max;

  if(d_minCutoff > d_maxCutoff)
    std::swap(d_minCutoff, d_maxCutoff);
}



/*!
  \sa      setCutoffs(), setMinCutoff(), setMaxCutoff(), minCutoff(),
           maxCutoff()
*/
void com::Classifier::resetCutoffs()
{
  d_minCutoff = d_min;
  d_maxCutoff = d_max;
}



void com::Classifier::resetMinCutoff()
{
  d_minCutoff = d_min;
}



void com::Classifier::resetMaxCutoff()
{
  d_maxCutoff = d_max;
}



/*!
  \param   v Value to classify.
  \return  Class index of value \a v.
  \sa      classBorder()
  \warning The result is undefined if the number classes == 0.

  The class index returned is the index of the class whose border is greater
  of equal to \a v. Values larger than the upper class border are assigned
  to the highest class.

  Returned class indices range from 0 to nr_of_classes - 1.
*/
size_t com::Classifier::classIndex(REAL8 v) const
{
#ifdef DEBUG_DEVELOP
  assert(nrClasses() > 0);
#endif

  const_iterator it = std::upper_bound(begin() + 1, end(), v);
  if(it != end())
    return it - (begin() + 1);
  else
    return nrClasses() - 1;
}



/*!
  \param   i Class index.
  \return  Upper class border.
  \warning The returned value is undefined if \a i >= nrClasses().
  \sa      classIndex()
*/
REAL8 com::Classifier::classBorder(size_t i) const
{
  assert(i < nrBorders());

  return d_borders[i];
}



/*!
  \param   n Number of class borders to calculate.
  \warning Only after classify() of classify(size_t) has been called the value
           set here and the value returned by nrClasses() are in sync.
  \sa      nrClasses()
*/
void com::Classifier::setNrClasses(size_t n)
{
  d_nrClasses = n;
}



/*!
  \param   v New minimum cutoff value.
  \sa      setMaxCutoff(), setCutoffs(), setExtremes(), minCutoff(), maxCutoff()
*/
void com::Classifier::setMinCutoff(REAL8 v)
{
  d_minCutoff = v;
}



/*!
  \param   v New maximum cutoff value.
  \sa      setMinCutoff(), setCutoffs(), setExtremes(), minCutoff(), maxCutoff()
*/
void com::Classifier::setMaxCutoff(REAL8 v)
{
  d_maxCutoff = v;
}



void com::Classifier::setMode(Mode m)
{
  d_mode = m;
}



com::Classifier::Mode com::Classifier::mode() const
{
  return d_mode;
}



//! Merges properties of \a classifier with this classifier.
/*!
  \param     classifier Object to use properties from.
  \exception .
  \warning   Does not call classify().

  The most extreme values of the extreme values of *this and \a classifier
  are set as the extreme values of *this. Other stuff is untouched.
*/
void com::Classifier::merge(Classifier const& classifier)
{
  if(extremesAreValid()) {
    if(classifier.extremesAreValid()) {
      setExtremes(std::min(min(), classifier.min()),
                  std::max(max(), classifier.max()));
    }
  }
  else {
    if(classifier.extremesAreValid()) {
      setExtremes(classifier.min(), classifier.max());
    }
  }
}



bool com::Classifier::extremesAreValid() const
{
  return
    !(pcr::isMV(d_min) || boost::math::isnan(d_min)) &&
    !(pcr::isMV(d_max) || boost::math::isnan(d_max));
}



bool com::Classifier::cutoffsAreValid() const
{
  return
    !(pcr::isMV(d_minCutoff) || boost::math::isnan(d_minCutoff)) &&
    !(pcr::isMV(d_maxCutoff) || boost::math::isnan(d_maxCutoff));
}



bool com::operator==(Classifier const& lhs, Classifier const& rhs)
{
  return lhs.equals(rhs);
}



bool com::operator!=(Classifier const& lhs, Classifier const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------

/*!
  \var com::Classifier::Algorithm com::Classifier::INVALID_ALGORITHM
  No classification algorithm set.

  \warning Don't call classify() or classify(size_t) if the algorithm()
           function returns this value.
*/

/*!
  \var com::Classifier::Algorithm com::Classifier::LIN

  Classification performed by com_LinClassifier object.
*/

/*!
  \var com::Classifier::Algorithm com::Classifier::LOG

  Classification performed by com_LogClassifier object.
*/

/*!
  \var com::Classifier::Algorithm com::Classifier::TLOG

  Classification performed by com_TLogClassifier object.
*/



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


