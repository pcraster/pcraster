#ifndef INCLUDED_CALC_ARRAYDEFVECTOR
#define INCLUDED_CALC_ARRAYDEFVECTOR

#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace calc {

class InfoScript;
class ArrayDefinition;

//! array properties of an parameter
/*! if a parameter is not an array then d_vector is 0-lenght
 */
class ArrayDefVector {
private:
  std::vector<const ArrayDefinition* > d_vector;
  std::map<std::vector<size_t>,size_t> d_map2flatIndex;
public:
  //! single index elemement
  typedef std::vector<const class IndexParameterConstant *> Index;


  typedef std::vector<std::vector<size_t> > IndexVector;

  //! vector of active indices
  IndexVector d_indices;

public:
  // CREATORS
  //! creator for array
  ArrayDefVector(
   const std::vector<const ArrayDefinition* >&vector);

  //! creator for non-array
  ArrayDefVector();

  // ACCESSORS

  //! string as in "[A][B][C]"
  std::string name() const;

  const ArrayDefinition *operator[](size_t i) const;

  //! number of dimensions
  size_t size() const;

  const Index element(size_t n) const;
  std::string outputSuffix(size_t i)const;

  bool operator==(const ArrayDefVector& a) const;

  //! total number of elements
  size_t nrElements() const;

  //! is it an array
  bool isArray() const;
  void print(InfoScript& is) const;

  size_t toLinear(std::vector<size_t>& ind) const;
};

}

#endif
