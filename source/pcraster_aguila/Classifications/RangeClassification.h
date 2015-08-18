#ifndef INCLUDED_RANGECLASSIFICATION
#define INCLUDED_RANGECLASSIFICATION



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_CLASSIFICATION
#include "Classification.h"
#define INCLUDED_CLASSIFICATION
#endif



namespace ag {
  // RangeClassification declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  Values equal to the lower border of a class belong to the class. Values
  equal to the upper border of a class belong to the higher class.

  \sa        .
*/
template<typename T>
class RangeClassification: public Classification<T>
{

  friend class RangeClassificationTest;

public:

  //! Types of classification algorithms.
  enum Algorithm {
    Linear,
    Log10,
  };

private:

  //! Minimum attribute value.
  T                _min;

  //! Maximum attribute value.
  T                _max;

  //! Classification algorithm.
  Algorithm        _algorithm;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RangeClassification ();

                   RangeClassification (T min,
                                        T max,
                                        size_t nrClasses,
                                        Algorithm algorithm=Linear);

                   RangeClassification (T min,
                                        T max,
                                        T minCutoff,
                                        T maxCutoff,
                                        size_t nrClasses,
                                        Algorithm algorithm=Linear);

  /* virtual */    ~RangeClassification();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             classify            (T minCutoff,
                                        T maxCutoff,
                                        size_t nrClasses,
                                        Algorithm algorithm=Linear);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           index               (T value) const;

  T                min                 () const;

  T                max                 () const;

  T                minCutoff           () const;

  T                maxCutoff           () const;

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
