#ifndef INCLUDED_CALC_SUBPARAMETER
#define INCLUDED_CALC_SUBPARAMETER

#ifndef INCLUDED_CALC_PARAMETER
#include "calc_parameter.h"
#define INCLUDED_CALC_PARAMETER
#endif

#ifndef INCLUDED_CALC_ARRAYDEFVECTOR
#include "calc_arraydefvector.h"
#define INCLUDED_CALC_ARRAYDEFVECTOR
#endif

namespace calc {

class Position;
class FileWriter;
class WriteInfo;
class ParsPar;
class StatementBlock;

//! parameter with <B>sub</B>scripting, can be an array of values or single value
/*!
   this type of parameters can be written out
 */
class  SubParameter : public Parameter {
 private:
  //! point where written.
  /*! d_writeInfo describes <B>if and when</B> the parameter is
   *  written. d_reportPoint describes at what point.
   *  Thus if d_writeInfo says it is written, only then d_reportPoint
   *  is defined.
   *  The position
   *  is the position where of the left side symbol (p) in p = ...
   */
  const Position       *d_reportPoint;

  const WriteInfo      *d_writeInfo;

  const ArrayDefVector  d_subscript;

  //! parameter is initialized by its external name
  const bool d_input;

 protected:
  // CREATORS
   SubParameter( const ParsPar& par, bool constant, bool isInput);
  virtual ~SubParameter();

  virtual void moreValidation(
      const std::string& fileName) const;

 public:
   // MANIPULATORS

  //! also checks if parameter is only reported once
  void setReportPoint(const Position *leftLeaf,
                      const WriteInfo& w);
  // ACCESSORS
  size_t nrElements() const;
  bool isArray() const;
  const class ArrayDefVector& arrayDefVector() const;

  bool writeHere(const Position *assignPoint) const;
  bool reportedInDynamic() const;

  //! check if the file is written under a valid filename
  void validateOutputFileName() const;

  //! return name as "A[B][C]" string (name() returns A)
  std::string arrayName()const;

  //! return external name including array indices
  std::string outputFileName(size_t index)const;

  //! always defined
  const WriteInfo *writeInfo() const;

  bool isOutput() const;
  bool isInput() const;

  virtual void printSubSpecific(InfoScript& is)const;

  //! create correct 'sub-type' info
  virtual void setDataSubType(pcrxml::Data *d) const=0;

  pcrxml::Data *createXmlData() const;

};

}

#endif
