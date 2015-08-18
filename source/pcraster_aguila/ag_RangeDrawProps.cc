#include "ag_RangeDrawProps.h"
#include <boost/bind.hpp>
#include <boost/format.hpp>
#include <boost/lexical_cast.hpp>
#include "dev_ToString.h"
#include "pcrtypes.h"
#include "ag_ColourSelector.h"
#include "com_classifier.h"
#include "com_rangemap.h"



/*!
  \file
  This file contains the implementation of the RangeDrawProps class.
*/


namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     c Classifier.
  \param     p Palette.
*/
RangeDrawProps::RangeDrawProps(
         std::string const& title,
         com::RawPalette const* p,
         com::Classifier* rawValueClassifier,
         com::Classifier* displayValueClassifier)

  : DrawProps(title, p),
    _drawerType(COLOURFILL),
    _probabilityScale(CumulativeProbabilities)

{
  assert(rawValueClassifier);

  if(displayValueClassifier) {
    assert(rawValueClassifier->nrClasses() ==
         displayValueClassifier->nrClasses());
  }

  pushClassifier(rawValueClassifier, displayValueClassifier);
  _nrClasses = rawValueClassifier->nrClasses();

  reMapColours();
  assignLabels();
}



RangeDrawProps::RangeDrawProps(
         RangeDrawProps const& properties)

  : DrawProps(properties),
    _classifiers(properties._classifiers),
    // _rawValueClassifier(properties._rawValueClassifier),
    // _displayValueClassifier(properties._displayValueClassifier),
    _drawerType(properties._drawerType),
    _minColour(properties._minColour),
    _maxColour(properties._maxColour),
    _probabilityScale(properties._probabilityScale)

{
}



//! Destructor.
/*!
  The classifier and the palette are for use only and won't be deleted here.

  \todo  Why don't we own the classifiers? Classifiers added with push are not
         deleted anywhere now! Check assignment and so on.
*/
RangeDrawProps::~RangeDrawProps()
{
}



//! Assignment operator.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
RangeDrawProps& RangeDrawProps::operator=(
         RangeDrawProps const& rhs)
{
  if(this != &rhs) {
    static_cast<DrawProps&>(*this) = rhs;
    _classifiers = rhs._classifiers;
    // _rawValueClassifier = rhs._rawValueClassifier;
    // _displayValueClassifier = rhs._displayValueClassifier;
    _drawerType = rhs._drawerType;
    _minColour = rhs._minColour;
    _maxColour = rhs._maxColour;
    _probabilityScale = rhs._probabilityScale;
  }

  assert(*this == rhs);

  return *this;
}



bool RangeDrawProps::equals(
         RangeDrawProps const& rhs) const
{
  if(static_cast<DrawProps const&>(*this) != rhs ||
         _drawerType != rhs._drawerType) {
    return false;
  }

  if(_minColour != rhs._minColour || _maxColour != rhs._maxColour) {
    return false;
  }

  if(_probabilityScale != rhs._probabilityScale) {
    return false;
  }

  if(_classifiers.size() != rhs._classifiers.size()) {
    return false;
  }

  for(size_t i = 0; i < _classifiers.size(); ++i) {
    ClassifierTuple const& lhsTuple = _classifiers[i];
    ClassifierTuple const& rhsTuple = rhs._classifiers[i];

    com::Classifier const* lhsClassifier;
    com::Classifier const* rhsClassifier;

    lhsClassifier = boost::get<0>(lhsTuple);
    rhsClassifier = boost::get<0>(rhsTuple);

    if((lhsClassifier && !rhsClassifier) ||
       (!lhsClassifier && rhsClassifier)) {
      return false;
    }

    if(lhsClassifier && rhsClassifier) {
      if(*lhsClassifier != *rhsClassifier) {
        return false;
      }
    }

    lhsClassifier = boost::get<1>(lhsTuple);
    rhsClassifier = boost::get<1>(rhsTuple);

    if((lhsClassifier && !rhsClassifier) ||
       (!lhsClassifier && rhsClassifier)) {
      return false;
    }

    if(lhsClassifier && rhsClassifier) {
      if(*lhsClassifier != *rhsClassifier) {
        return false;
      }
    }
  }

  return true;

/*
  if(_displayValueClassifier) {
    if(!rhs._displayValueClassifier) {
      return false;
    }

    return static_cast<DrawProps const&>(*this) == rhs &&
         *_rawValueClassifier == *rhs._rawValueClassifier &&
         *_displayValueClassifier == *rhs._displayValueClassifier &&
         _drawerType == rhs._drawerType;
  }
  else {
    if(rhs._displayValueClassifier) {
      return false;
    }

    return static_cast<DrawProps const&>(*this) == rhs &&
         *_rawValueClassifier == *rhs._rawValueClassifier &&
         _drawerType == rhs._drawerType;
  }
  */
}



void RangeDrawProps::setDisplayValueClassifier(com::Classifier* classifier)
{
  assert(classifier);
  ClassifierTuple& tuple(_classifiers.back());
  assert(!boost::get<1>(tuple));
  boost::get<1>(tuple) = classifier;
  assert(boost::get<1>(tuple) == classifier);
  // _displayValueClassifier = classifier;
}



void RangeDrawProps::unsetDisplayValueClassifier()
{
  ClassifierTuple& tuple(_classifiers.back());
  // Assumes the caller deletes the classifier.
  boost::get<1>(tuple) = 0;
  assert(!boost::get<1>(tuple));
  // _displayValueClassifier = 0;
}



void RangeDrawProps::setNrClasses(size_t nrClasses)
{
  com::Classifier* raw(rawValueClassifier());

  raw->setNrClasses(nrClasses);

  com::Classifier* display(displayValueClassifier());

  if(display) {
    display->setNrClasses(nrClasses);
  }
}



void RangeDrawProps::setMaxCutoff(double maxCutoff)
{
  com::Classifier* raw(rawValueClassifier());

  raw->setMaxCutoff(maxCutoff);

  com::Classifier* display(displayValueClassifier());

  if(display) {
    display->setMaxCutoff(maxCutoff);
  }
}



void RangeDrawProps::setMinCutoff(double cutoff)
{
  com::Classifier* raw(rawValueClassifier());

  raw->setMinCutoff(cutoff);

  com::Classifier* display(displayValueClassifier());

  if(display) {
    display->setMinCutoff(cutoff);
  }
}



void RangeDrawProps::setCutoffs(
         double minCutoff,
         double maxCutoff)
{
  com::Classifier* raw(rawValueClassifier());

  raw->setCutoffs(minCutoff, maxCutoff);

  com::Classifier* display(displayValueClassifier());

  if(display) {
    display->setCutoffs(minCutoff, maxCutoff);
  }
}



void RangeDrawProps::resetMaxCutoff()
{
  com::Classifier* raw(rawValueClassifier());
  com::Classifier* display(displayValueClassifier());
  raw->resetMaxCutoff();

  if(display) {
    display->resetMaxCutoff();
  }
}



void RangeDrawProps::resetMinCutoff()
{
  com::Classifier* raw(rawValueClassifier());
  com::Classifier* display(displayValueClassifier());
  raw->resetMinCutoff();

  if(display) {
    display->resetMinCutoff();
  }
}



void RangeDrawProps::resetCutoffs()
{
  com::Classifier* raw(rawValueClassifier());
  com::Classifier* display(displayValueClassifier());
  raw->resetCutoffs();

  if(display) {
    display->resetCutoffs();
  }
}



void RangeDrawProps::setMode(Mode mode)
{
  // _rawValueClassifier->setMode(mode);
  // if(_displayValueClassifier) {
  //   _displayValueClassifier->setMode(mode);
  // }

  com::Classifier* raw(rawValueClassifier());
  com::Classifier* display(displayValueClassifier());
  raw->setMode(mode);

  if(display) {
    display->setMode(mode);
  }
}



void RangeDrawProps::setProbabilityScale(
         ProbabilityScale scale)
{
  _probabilityScale = scale;
}



void RangeDrawProps::setAlgorithm(Algorithm algorithm)
{
  // _rawValueClassifier->installAlgorithm(algorithm);
  // if(_displayValueClassifier) {
  //   _displayValueClassifier->installAlgorithm(algorithm);
  // }


  com::Classifier* raw(rawValueClassifier());
  com::Classifier* display(displayValueClassifier());
  raw->installAlgorithm(algorithm);

  if(display) {
    display->installAlgorithm(algorithm);
  }
}



RangeDrawProps::Mode RangeDrawProps::mode() const
{
  // return _rawValueClassifier->mode();

  return rawValueClassifier()->mode();
}



RangeDrawProps::ProbabilityScale RangeDrawProps::probabilityScale() const
{
  return _probabilityScale;
}



bool RangeDrawProps::cutoffsAreValid() const
{
  com::Classifier const* raw(rawValueClassifier());
  com::Classifier const* display(displayValueClassifier());

  return display ? display->cutoffsAreValid() : raw->cutoffsAreValid();
}



double RangeDrawProps::min() const
{
  com::Classifier const* raw(rawValueClassifier());
  com::Classifier const* display(displayValueClassifier());

  return display ? display->min() : raw->min();
}



double RangeDrawProps::max() const
{
  com::Classifier const* raw(rawValueClassifier());
  com::Classifier const* display(displayValueClassifier());

  return display ? display->max() : raw->max();
}



double RangeDrawProps::minCutoff() const
{
  com::Classifier const* raw(rawValueClassifier());
  com::Classifier const* display(displayValueClassifier());

  return display ? display->minCutoff() : raw->minCutoff();
}



double RangeDrawProps::maxCutoff() const
{
  com::Classifier const* raw(rawValueClassifier());
  com::Classifier const* display(displayValueClassifier());

  return display ? display->maxCutoff() : raw->maxCutoff();
}



RangeDrawProps::Algorithm RangeDrawProps::algorithm() const
{
  // return _rawValueClassifier->algorithm();

  return rawValueClassifier()->algorithm();
}



void RangeDrawProps::classify()
{
  com::Classifier* raw(rawValueClassifier());
  com::Classifier* display(displayValueClassifier());
  raw->classify();

  if(display) {
    display->classify();
    assert(display->nrClasses() == raw->nrClasses());
  }

  // Don't forget to update our copy of nrClasses!
  _nrClasses = rawValueClassifier()->nrClasses();

  reMapColours();
  assignLabels();
}



void RangeDrawProps::reMapColours()
{
  if(rawValueClassifier()->nrClasses() == 0) {
    assert(!palette()->empty());
    _minColour = qt::RgbTupleToQColor(*(palette()->begin()), palette()->max());
    _maxColour = _minColour;
  }
  else {
    _colours = mapEqualInterval(*palette(), rawValueClassifier()->nrClasses());
    assert(!_colours.empty());
    _minColour = _colours.front();
    _maxColour = _colours.back();
  }
}



void RangeDrawProps::assignLabels()
{
  if(nrClasses() > 0) {

    _labels.resize(nrClasses() + 1);

    com::Classifier* raw(rawValueClassifier());
    com::Classifier* display(displayValueClassifier());

    if(display) {

      for(size_t i = 0; i <= nrClasses(); ++i) {
        _labels[i] = dev::toString(display->classBorder(i));
      }
    }
    else {

      for(size_t i = 0; i <= nrClasses(); ++i) {
        _labels[i] = dev::toString(raw->classBorder(i));
      }
    }
  }
}



const std::vector<double>& RangeDrawProps::rawClassBorders() const
{
  return rawValueClassifier()->borders();
}



size_t RangeDrawProps::rawClassIndex(double value) const
{
  return rawValueClassifier()->classIndex(value);
}



//! Returns a collection of classborders converted by the display value classifier if set.
/*!
  \return    Collection of class borders.

  Returns result of rawClassBorders() if no display value classifier is set.
*/
std::vector<double> RangeDrawProps::classBorders() const
{
  std::vector<double> borders = rawClassBorders();

  for(std::vector<double>::iterator it = borders.begin(); it != borders.end();
         ++it) {
    *it = rawToDisplay(*it);
  }

  return borders;
}



//! Converts raw value to display value.
/*!
  \param     value Raw value as present in the source.
  \return    Display value.

  This function does nothing if no classifier for display values is installed.
*/
double RangeDrawProps::rawToDisplay(double value) const
{
  com::Classifier const* raw(rawValueClassifier());
  com::Classifier const* display(displayValueClassifier());

  if(display) {
    com::RangeMap<double, double> mapper(
         raw->min(), raw->max(), display->min(), display->max());
    value = mapper.map(value);
  }

  return value;
}



void RangeDrawProps::setDrawerType(DrawerType type)
{
  _drawerType = type;
}



DrawerType RangeDrawProps::drawerType() const
{
  return _drawerType;
}



size_t RangeDrawProps::nrClassesRequested() const
{
  return rawValueClassifier()->nrClassesRequested();
}



void RangeDrawProps::merge(RangeDrawProps const& properties)
{
  rawValueClassifier()->merge(*properties.rawValueClassifier());

  com::Classifier* lhsDisplay(displayValueClassifier());
  com::Classifier const* rhsDisplay(properties.displayValueClassifier());

  if(lhsDisplay && rhsDisplay) {
    lhsDisplay->merge(*rhsDisplay);
  }
}



std::string RangeDrawProps::label(
         REAL4 const& value) const
{
  std::string result = "mv";

  if(!pcr::isMV(value)) {
    result = (boost::format("%1%") % rawToDisplay(value)).str();
  }

  return result;
}



com::Classifier* RangeDrawProps::displayValueClassifier()
{
  assert(!_classifiers.empty());

  return boost::get<1>(_classifiers.back());
}



const com::Classifier* RangeDrawProps::displayValueClassifier() const
{
  assert(!_classifiers.empty());

  return boost::get<1>(_classifiers.back());
}



com::Classifier* RangeDrawProps::rawValueClassifier()
{
  assert(!_classifiers.empty());

  return boost::get<0>(_classifiers.back());
}



const com::Classifier* RangeDrawProps::rawValueClassifier() const
{
  assert(!_classifiers.empty());

  return boost::get<0>(_classifiers.back());
}



void RangeDrawProps::pushClassifier(
         com::Classifier* rawValueClassifier,
         com::Classifier* displayValueClassifier)
{
  _classifiers.push_back(ClassifierTuple(
         rawValueClassifier, displayValueClassifier));
}



RangeDrawProps::ClassifierTuple RangeDrawProps::popClassifier()
{
  assert(!_classifiers.empty());
  assert(_classifiers.size() > 1);

  ClassifierTuple tuple = _classifiers.back();
  _classifiers.pop_back();

  return tuple;
}



RangeDrawProps::ClassifierTuples const& RangeDrawProps::classifiers() const
{
  return _classifiers;
}



RangeDrawProps::ClassifierTuples& RangeDrawProps::classifiers()
{
  return _classifiers;
}



QColor const& RangeDrawProps::minColour() const
{
  return _minColour;
}



QColor const& RangeDrawProps::maxColour() const
{
  return _maxColour;
}



QColor const& RangeDrawProps::colour(
         double value) const
{
  assert(!pcr::isMV(value));

  if(value < minCutoff()) {
    return minColour();
  }
  else if(value > maxCutoff()) {
    return maxColour();
  }
  else {
    return colourByIndex(rawClassIndex(value));
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

bool operator==(RangeDrawProps const& lhs, RangeDrawProps const& rhs)
{
  return lhs.equals(rhs);
}



bool operator!=(RangeDrawProps const& lhs, RangeDrawProps const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag
