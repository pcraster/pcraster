#ifndef INCLUDED_CALC_ISCRIPT
#define INCLUDED_CALC_ISCRIPT

#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif
#ifndef INCLUDED_CALC_PROGRESSPULSE
#include "calc_progresspulse.h"
#define INCLUDED_CALC_PROGRESSPULSE
#endif

namespace geo {
  class RasterSpace;
}

namespace calc {

class ModelLink;
class FieldMapInputParameter;
class ParsPar;
class SubParameter;
class GridMap;
class IoFieldStrategy;
class StackReader;
class Compressor;

//! interface to script
/*! interface class of Script
 */
class IScript {
 private:
   //! the current timestep, 0 means initial part or static script
   int    d_timeStep;

  virtual void checkClone(const std::string& mapFileName)=0/* THROW (StrErrorExcep)*/;

  //! forbidden
  IScript(const IScript& copy);

 protected:
  //! 0 for start in initial
  size_t  d_timerStart;
  //! 1 for end of initial/static model
  size_t  d_timerEnd;
  //! 0 for marking, no timer section parsed
  size_t  d_timerSlice;

 public:
   // CREATORS
  IScript();

  virtual ~IScript();


  //! incr timer during execution, only DynamicSection::ExecuteBlock does this
  void nextTimeStep();

   virtual void    addSymbol(class UserSymbol *newPar) =0;
   virtual void    setReportFound()=0;

  //! return a new allocated format appropriate stack reader
  virtual const StackReader* createStackReader(const std::string& stackName)=0;

  void cmpToClone(const std::string& mapFileName)const;

  // ACCESSORS
  //! data is altered if a debug assertion is encountered
  virtual unsigned char *areaMask() const=0;

   virtual const class Report *reportDefault()const=0;

   virtual bool allIsWritten()const=0;

  //! find if name has a binding
  /*! Binding returns 0 if no binding found, otherwise the (right-)string of the binding
   */
  virtual const Symbol      *findBinding(const std::string& name) const=0;

  //! find symbol, message user if symbol is not like expected
  /*! typesExpected is a set of types allowd
   *  return 0 if not exist and mustExist is false
   */
  virtual class UserSymbol *findSymbol(const class Symbol* sym,
    VS typesExpected, bool mustExist) const =0;

        //! find and verifies correct use of parameter
  /*! returns 0 if the parameter does not yet exit in the symbol
   * table. If it does not exist, it does check if par is available
   * on the filesystem in the case of non-array. Array's that does
   * not exist, or are used with the wrong descriptor will throw an
   * exception.
   */
  virtual SubParameter *findRightParameter(
      const ParsPar& par,
    VS expectedVs ) const =0;

        //! find and verifies correct use of parameter
  /*! returns 0 if the parameter does not yet exists in the symbol table
   * Parameters that does
   * exist, but are used with the wrong descriptor will throw an
   * exception.
   */
  virtual SubParameter *findLeftParameter(
    const ParsPar& par,
    VS expectedVs) const=0;

  size_t currentTimeStep()   const;
  size_t nrTimeSteps()       const;
  bool   isDynamicModel()    const;

  virtual bool   writeEachTimeStep() const=0;
  virtual bool  debugMvAssignments() const=0;
  virtual bool  zeroCompression() const=0;
  virtual std::string  debugMapName() const=0;

  virtual FieldMapInputParameter *addExternalFieldLeaf(const ParsPar& par)=0;

  //! only UserModelLink::initExecute needs this
  virtual const geo::RasterSpace& rasterSpace() const=0;

  virtual const Compressor&       compressor()  const=0;

  virtual GridMap *createMap(const std::string& d_fileName, VS vs) const=0;

  virtual const IoFieldStrategy& ioFieldStrategy() const=0;

  virtual void removeOutputObject(const std::string& objName) const=0;

  virtual void  updateProgress(ProgressPulse p,int step=-1)=0;
  virtual void  processFileOutputValue(double val)=0;

  virtual bool   esriGridIO() const=0;

  //! RunDirectory::inputFilePath()
  virtual std::string  inputFilePath(const std::string& fileName) const=0;
  //! RunDirectory::outputFilePath()
  virtual std::string outputFilePath(const std::string& fileName) const=0;

};

}

#endif
