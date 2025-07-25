#include "stddefx.h"
#include "calc_subparameter.h"
#include "calc_arraydefvector.h"
#include "calc_file.h"   // Validation
#include "calc_writeinfo.h"
#include "calc_infoscript.h"
#include "calc_filewriter.h"
#include "calc_parspar.h"
#include "calc_iscript.h"
#include "pcrgenxml_data.h"
#include "com_exception.h"
#include "calc_position.h"

calc::SubParameter::SubParameter(const calc::ParsPar& par, bool constant, bool input):
  calc::Parameter(par,constant),
  d_reportPoint(nullptr),
  d_writeInfo(nullptr),
  d_subscript(par.descriptor()),
  d_input(input)
{
}

calc::SubParameter::~SubParameter()
{
  delete d_writeInfo;
}

bool calc::SubParameter::isArray() const
{
  return d_subscript.isArray();
}

size_t calc::SubParameter::nrElements() const
{
  return d_subscript.nrElements();
}

const class calc::ArrayDefVector& calc::SubParameter::arrayDefVector() const
{ return d_subscript; }



/*!
  \todo
    no clue on condition for posError
*/
void calc::SubParameter::validateOutputFileName() const
{
  for(size_t i=0; i < nrElements();i++) {
    std::string fileName(outputFileName(i));
    try {
      moreValidation(fileName);
    } catch(const com::Exception& err) {
     // if: check on the (rare) case that fileName will be a timeseries file
     //    but is now an ESRI GRID?
     if (!(scriptConst().esriGridIO() && err.errorNr() == com::E_ISDIR)) {
      // pcrcalc/test62
      symError(err);
     }
    }
 }
}

//! throw com::Exception if name validation fails
/*! can be reimplemented bug should call this one
    assumes subparameter is a file not a directory as is the case in ESRI mode
 */
void calc::SubParameter::moreValidation(
  const std::string& fileName) const
{
    calc::File f(fileName);
    f.validateOutputName();
}

std::string calc::SubParameter::outputFileName(size_t index)const
{
  return externalName()+d_subscript.outputSuffix(index);
}

void calc::SubParameter::setReportPoint(
  const Position *writtenHere,
  const WriteInfo& w)
{
  if (d_writeInfo) {
    if (w.hasReportPrefix() && d_writeInfo->hasReportPrefix()) {
    PRECOND(d_reportPoint);
    writtenHere->throwError("Report already on "+quote(userName())+" done previous ("
        +d_reportPoint->text()+")");
    }
  } else {
    // first encounter of assigning to this parameter
    d_writeInfo = new calc::WriteInfo(w);
  }
  if (! w.isWritten())
    return;
  validateOutputFileName();
  // since we write the LAST assignment
  // set d_writeInfo to this last one
  delete d_writeInfo;
  d_writeInfo = new calc::WriteInfo(w);

  d_reportPoint = writtenHere;
}

/*!
 * \todo
 *   bad, refactor completely out, doing ptr comparision is risky
 *   if changes by copying data
 */
bool calc::SubParameter::writeHere(const Position *assignPoint) const
{
  return d_reportPoint == assignPoint;
}

bool calc::SubParameter::reportedInDynamic() const
{
  return d_writeInfo && d_writeInfo->inDynamic();
}

//! will this parameter be written as result?
bool calc::SubParameter::isOutput() const
{
  return d_writeInfo && d_writeInfo->isWritten();
}

//! is parameter initialized by its external name
bool calc::SubParameter::isInput() const
{
  return d_input;
}

std::string calc::SubParameter::arrayName()const
{
  return name()+d_subscript.name();
}

void calc::SubParameter::printSubSpecific(calc::InfoScript& is)const
{
  is.stream() << "no sub-specifics<BR>";
}

const calc::WriteInfo *calc::SubParameter::writeInfo() const
{
  return d_writeInfo;
}

/*!
   \todo
      IoBoth is missing
   \todo
      Introduce SubParameter::isIoBoth?
   \todo
      Xml listing skips ones with multiple vs's
   \bug
      Since Xml listing skips ones with multiple vs's, it
      will ignore csf version 1 maps
*/
pcrxml::Data *calc::SubParameter::createXmlData() const
{
   if (nrInSet(symbolType()) != 1)
     return nullptr;
   auto *d= new pcrxml::Data();
   setDataSubType(d);
   setName(d);

   if (isInput()) {
     d->ioType = pcrxml::IoType::Input;
   } else {
       if (isOutput())
        d->ioType = pcrxml::IoType::Output;
       else {
         if (isConstantBinding())
          d->ioType = pcrxml::IoType::Constant;
         else
          d->ioType = pcrxml::IoType::None;
      }
   }
   return d;
}
