#include "stddefx.h"

#ifndef INCLUDED_CALC_INDEXTABLE
# include "calc_indextable.h"
# define INCLUDED_CALC_INDEXTABLE
#endif

#ifndef INCLUDED_CALC_LEXVALUEFILE
# include "calc_lexvaluefile.h"
# define INCLUDED_CALC_LEXVALUEFILE
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_CALC_INDEXPARAMETERCONSTANT
#include "calc_indexparameterconstant.h"
#define INCLUDED_CALC_INDEXPARAMETERCONSTANT
#endif

#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif

#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif


#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

namespace calc {
class LexIndexTable : public LexValueFile {
  IndexTable* d_to;
 public:
  LexIndexTable(std::ifstream* f, IndexTable *to)
    : LexValueFile(f),d_to(to) {};
  void processLine(const Line& line, int lineNr);
 };
}

/*! throws calc::IndexTable::AddRecord expections
 */
void calc::LexIndexTable::processLine(const Line& line, int lineNr)
{
  d_to->addRecord(line,lineNr);
}

calc::IndexTable::IndexTable(
  const calc::Symbol& name,
  const calc::ArrayDefVector& array):
  Parameter(name,true),
  d_array(array)
{
   std::ifstream  file(externalName().c_str(),std::ifstream::in);
   POSTCOND(file.is_open());
   calc::LexIndexTable l(&file,this);
  try {
   l.process();
  }
  catch(com::Exception& msg) {
     posError("While reading "+quote(userName())+":\n"+msg.messages());
  }
}

VS calc::IndexTable::symbolType() const
{
  return VS_INDEXTABLE;
}

std::string calc::IndexTable::arrayDefName() const
{
  return d_array.name();
}

//! prepare and throw a syntax error found in the index table
/*! throws a com::Exception denoting syntax error on line with file name and line nr
 */
void calc::IndexTable::throwError(
  int lineNr,
  std::ostringstream& what) const
{
  std::ostringstream  msg;
  msg << externalName();
  msg << ":";
  msg << lineNr;
  msg << ":";
  msg << what.str();
  throw com::Exception(msg.str());
}

//! add a record to the index table <B>THROWS</B>
/*! Add the \a line from line nr \a lineNr the file externalName()
 *  Throw com::Exception if syntax error on line with file name and line nr
 */
void calc::IndexTable::addRecord(const std::vector<std::string>& line,int lineNr)
{
  Value value;
  value.d_value=line.back();
  value.d_lineNr=lineNr;
  size_t expectedSize = d_array.size()+2; // 1+parameter name 1+result
  if (line.size() != expectedSize) // 1+parameter name 1+result
  {
    std::ostringstream  msg;
    msg << " expecting ";
    msg << expectedSize;
    msg << " items read ";
    msg << line.size();
    msg << " items";
    throwError(lineNr,msg); // pcrcalc/test281
  }
   Pair e(Key(line.begin(),line.end()-1) ,value);
  std::pair<Table::iterator,bool> p = d_table.insert(e);
  if (!p.second) {
    // pcrcalc/test282
    std::ostringstream  msg;
    Table::iterator fd =p.first; // first definition if error, or (new) position
    msg << " key already defined at line ";
    msg << fd->second.d_lineNr;
    throwError(lineNr,msg);
  }
}

/*! throws com::Exception if key not found
 */
const calc::IndexTable::Value& calc::IndexTable::find(
  const std::string& parExtName,
  size_t i) const
{
    const calc::ArrayDefVector::Index& ind(d_array.element(i));
    std::vector<std::string> key;
    key.push_back(parExtName);
    for(size_t j=0; j < ind.size(); j++)
      key.push_back(ind[j]->externalName());
    Table::const_iterator p=d_table.find(key);
    if (p == d_table.end()) {
      std::string keyStr(parExtName);
      for(size_t j=0; j < ind.size(); j++)
        keyStr += "["+ind[j]->externalName()+"]";
      throw com::Exception("No value found for "+keyStr); //pcrcalc/test280
    }
    return p->second;
}

void calc::IndexTable::fieldNrValues(const calc::BindedSymbol& par,VS vs,
    std::vector<double>& vals) const
{
  size_t n = d_array.nrElements();
  PRECOND(!vals.size());
  vals.reserve(n);
  for (size_t i=0;i < n; i++) {
    const Value& val = find(par.externalName(),i);
    double valD;
    if ((!CnvrtDouble(&valD,val.d_value.c_str()))
        || !isIn(vs,vsOfNumber(valD)) ) { // pcrcalc/test283
      std::ostringstream  msg;
      msg << quote(val.d_value);
      msg << " is not a legal ";
      msg << toString(vs);
      msg << " non-spatial";
      throwError(val.d_lineNr,msg);
    }
    vals.push_back(valD);
  }
}

VS calc::IndexTable::fieldMapValues(const calc::BindedSymbol& par,
    std::vector<std::string>& vals) const
{
  size_t n = d_array.nrElements();
  PRECOND(!vals.size());
  vals.reserve(n);
  VS mapVs = VS_FIELD;
  for (size_t i=0;i < n; i++) {
    const Value& val = find(par.externalName(),i);
    std::string  fileName(val.d_value);
    try {
      double valD;
      if (CnvrtDouble(&valD,fileName.c_str())) // bummer, its a number
          throw com::Exception(" is not a map"); // pcrcalc/test306
      fileName = scriptConst().inputFilePath(fileName);
      VS newVs = expectedFileType(fileName,VS_FIELD); // pcrcalc/test307
      if (!isIn(newVs,mapVs)) {
          throw com::Exception(" is a "+toString(newVs)
                              +" map, previous values are of type "+toString(mapVs)); // pcrcalc/test308
      }
      mapVs=newVs;
      scriptConst().cmpToClone(fileName); // pcrcalc/test309
    } catch(com::Exception& strExcep) {
      std::ostringstream  msg;
      msg << quote(fileName);
      msg << " ";
      msg << strExcep.messages();
      throwError(val.d_lineNr,msg);
    }
    vals.push_back(fileName);
  }
  // if no active indices (n == 0) we do not have a type
  POSTCOND(nrInSet(mapVs) == 1 || n == 0);
  return mapVs;
}

//! only get the names to verify they are there
void calc::IndexTable::tableNameVerify(const calc::BindedSymbol& par) const
{
  size_t n = d_array.nrElements();
  for (size_t i=0;i < n; i++) {
    const Value& val = find(par.externalName(),i);
    double valD;
    if (CnvrtDouble(&valD,val.d_value.c_str())) {
      // pcrcalc/test286
      std::ostringstream msg;
      msg << quote(val.d_value);
      msg << " is not a legal tablename";
      throwError(val.d_lineNr,msg);
    }
    std::string fileName = scriptConst().inputFilePath(val.d_value);
    try {
      expectedFileType(fileName,VS_TABLE); // pcrcalc/test285
    }
    catch(com::Exception& msg) {
      std::ostringstream  omsg;
      omsg << msg.messages();
      throwError(val.d_lineNr,omsg);
    }
  }
}

void calc::IndexTable::nameValues(const calc::BindedSymbol& par, std::vector<const Value*>& values) const
{
  size_t n = d_array.nrElements();
  values.resize(n);
  for (size_t i=0;i < n; i++) {
    try {
      const Value& v = find(par.externalName(),i);
      values[i] = &v;
    } catch (com::Exception& msg) {
      POSTCOND(FALSE);
    }
  }
}

void calc::IndexTable::printSpecific(calc::InfoScript& is)const
{
  is.stream() << "Linked to: ";
  d_array.print(is);
  is.stream() << "<BR>";
}
