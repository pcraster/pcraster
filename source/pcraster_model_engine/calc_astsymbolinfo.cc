#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTSYMBOLINFO
#include "calc_astsymbolinfo.h"
#define INCLUDED_CALC_ASTSYMBOLINFO
#endif

// Library headers.
#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_CLONE
#include "com_clone.h"
#define INCLUDED_COM_CLONE
#endif
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif
// Module headers.
#ifndef INCLUDED_CALC_XMLDATATYPE
#include "calc_xmldatatype.h"
#define INCLUDED_CALC_XMLDATATYPE
#endif
#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"
#define INCLUDED_CALC_POSITIONNAME
#endif
#ifndef INCLUDED_CALC_SYMEXCEPTION
#include "calc_symexception.h"
#define INCLUDED_CALC_SYMEXCEPTION
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif


#ifndef INCLUDED_CALC_IOSTRATEGY
#include "calc_iostrategy.h"
#define INCLUDED_CALC_IOSTRATEGY
#endif

#ifndef INCLUDED_CALC_STACKINPUT
#include "calc_stackinput.h"
#define INCLUDED_CALC_STACKINPUT
#endif
#ifndef INCLUDED_CALC_ASTDEFINITION
#include "calc_astdefinition.h"
#define INCLUDED_CALC_ASTDEFINITION
#endif
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif
/*!
  \file
  This file contains the implementation of the ASTSymbolInfo class.
*/



//------------------------------------------------------------------------------


namespace calc {
  namespace detail {

  static ASTPar parDefaultCtor("error_ASTSymbolInfoDefaultCtor");

  /*
   * static std::string toString(ASTSymbolInfo const& si) {
   *   std::ostringstream os;
   *   os << si;
   *   return os.str();
   * }
   */

 } // namespace detail
} // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTSYMBOLINFO MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTSYMBOLINFO MEMBERS
//------------------------------------------------------------------------------

void calc::ASTSymbolInfo::init()
{
  d_firstAss               =0;
  d_dataType               = DataType();
  d_firstCodePresence      = &detail::parDefaultCtor;
  d_stackInput             =0;
  d_objectLinkConstructor  =0;
  d_hasInterface           =false;
  d_definitionRole         =NotSpecified;

  d_ioType
    =IOType(pcrxml::ModelInputType::Initial,pcrxml::ModelOutputType::Fixed);

  d_reportPosition         =RPNone;
  d_reportPar              =0;
  d_report                 =0;
  d_definition             =0;
  d_definitionCreation     =NotCreated;
}

void calc::ASTSymbolInfo::initCopy(const ASTSymbolInfo& rhs)
{
  d_firstAss               = rhs.d_firstAss;
  d_dataType               = rhs.d_dataType;
  d_firstCodePresence      = rhs.d_firstCodePresence;
  d_stackInput             = com::non0CopyCtor(rhs.d_stackInput);
  d_objectLinkConstructor  = rhs.d_objectLinkConstructor;
  d_hasInterface           = rhs.d_hasInterface;
  d_definitionRole         = rhs.d_definitionRole;
  d_ioType                 = rhs.d_ioType;
  d_reportPosition         = rhs.d_reportPosition;
  d_reportPar              = rhs.d_reportPar;
  d_report                 = rhs.d_report;
  d_definition             = com::non0CopyCtor(rhs.d_definition);
  d_definitionCreation     = rhs.d_definitionCreation;
}


calc::ASTSymbolInfo::ASTSymbolInfo()
{
  init();
}

//! ctor, for symbols found first in the ASTNode tree
calc::ASTSymbolInfo::ASTSymbolInfo(const DataType& dataType,
                                   const ASTPar  *first)
{
  init();
  d_dataType=dataType;
  d_firstCodePresence = first;
}

//! Assignment operator.
calc::ASTSymbolInfo& calc::ASTSymbolInfo::operator=(const ASTSymbolInfo& rhs)
{
    if (this != &rhs)
      initCopy(rhs);
    return *this;
}

//! Copy constructor.
calc::ASTSymbolInfo::ASTSymbolInfo(const ASTSymbolInfo& rhs)
{
  initCopy(rhs);
}



calc::ASTSymbolInfo::~ASTSymbolInfo()
{
 delete d_stackInput;
 delete d_definition;
}

void calc::ASTSymbolInfo::checkOneOutputVs() const
{
   //   pcrcalc38[a] and pcrcalc68
   const char *arg="an output data type ";
   VS vsToCheck=vs();
   if (vs() == VS_TSS) {
     vsToCheck=dataType().resultType();
     arg="a data type for second argument of timeoutput";
   }
   try {
    checkOneVs(vsToCheck,arg);
   } catch (const com::Exception& e) {
     throwAtFirst(e);
   }
}

std::string calc::ASTSymbolInfo::vsClashError(
  const calc::VSClash&  v,
  const std::string&         currentVerb) const
{
  std::ostringstream msg;
  if (firstIsCreation())
    msg << "defined as ";
  else
    msg << "used as "; // TODO pcrcalc501 must do this
   msg<<v.mustBeOneOf()<<" type on "
    <<firstText()<<" and "<< currentVerb
    << " here as "<<v.isOneOf()<<" type";
  return msg.str();
}

const char *calc::ASTSymbolInfo::str(DefinitionCreation dc)
{
    switch(dc) {
      case NotCreated: return "NotCreated?"; // error
      case Binding:    return "binding section";
      case Interface:  return "interface section";
      case Definition: return "xml definition wrapper";
      case Resolve:
      default:         return "resolve()";
    }
}

void calc::ASTSymbolInfo::resetDefinition(DefinitionCreation definitionCreation)
{
  if (d_definition && d_definitionCreation != definitionCreation) {
    std::ostringstream msg;
    msg << "duplicate definition in both " << str(d_definitionCreation)
        << " and " << str(definitionCreation);
    throwAtFirst(com::Exception(msg.str()));
  }
  d_definitionCreation = definitionCreation;
}

void calc::ASTSymbolInfo::setDefinition(const pcrxml::Definition& d)
{
  resetDefinition(Definition);
  d_definition = new pcrxml::Definition(d);

  // TODO set description

/*
 * // simple binding FTTB
 * std::string extName;
 * // TODO the memory part
 * // TODO accepting both input and output
 * if (d.scriptInput().present() && d.scriptInput()->external())
 *     extName=pcrxsd::toString<>(d.scriptInput()->external());
 * else {
 *   if (d.scriptOutput().present()) {
 *     d_definitionRole=Output;
 *     if (d.scriptOutput()->external())
 *       extName=pcrxsd::toString<>(d.scriptOutput()->external());
 *   }
 * }
 * if (!extName.empty())
 *   setExternalNameXXXX(extName);
 */

  if (d.field().present()) {
   pcrxml::FieldValueOrType const& f(d.field().get());

   DataType dt(xml2DataType(f));

   try {
    // other way around, to create correct messages
    dt.restrict(d_dataType);
   } catch(const VSClash& v) {
      PositionName pn("definition");
      throwSym(pn,vsClashError(v,"set"));
   } catch(const STClash& s) {
      PositionName pn("definition");
      const char *cfg   ="nonspatial";
      const char *script="spatial";
      if (s.spatialFound()) {
       cfg="spatial";script="nonspatial";
    }
      std::ostringstream msg;
      msg << "script requires "<< script<<" set here as " << cfg;
      throwSym(pn,msg.str());
    }
    // set as needed, not other way around
    d_dataType.restrict(dt);
  } // eo field part


}

/*! \brief resolve data requirements, finding and checking files, comparing
           against clone maps: Is it a map or a table/tss/indextable?
 \param ios  updated for clone info if found
 \throws VSClash if field input does not match  expectation, dataType is set
         to error in that case.
 \throws PosException if file not found, wrong file type etc.
 \todo   getting messy this code, maybe put each resolve in type: StackInput,
          LookupTable etc.
 */
void calc::ASTSymbolInfo::resolve(
        IOStrategy& ios)
{
  // if output, check if path is correct
  // TODO moet eigenlijk naar execute initialisatie
  if (reportPar())
    ios.checkOutputFilePath(*this);

  // only check the inputs FTTB

  if (vs()==VS_STATISTICS) // StatTable id
    return;
  if (vs()==VS_OBJECT)
    return;

  if (isConstant() || firstIsCreation()) // not input
    return; // done rest of code dealing with input

/*
 * if (d_hasInterface) {
 *   if (d_dataType.stEither())
 *     d_dataType.promoteToSpatial();
 *   return;
 * }
 */

  DataType newDt=d_dataType; // used in catch block

  try {
    if (vs()==VS_MAPSTACK) {
      //  not yet resolved
      if (!d_stackInput) {
        d_stackInput= ios.createStackInput(externalName(),d_dataType.mapStackType());
        if (d_stackInput)
          d_dataType.setResultType(VS_MAPSTACK,d_stackInput->fieldReturnVs());
      }
      return;
    }
    if (lookupTable())
      return;
    if (memoryInputId()!= noMemoryExchangeId()) {
      // memory input
      if (d_dataType.stEither()) {
        throw com::Exception("spatialType undecided (specify by field.spatialType)");
     }
      return;
    }

    std::string newExternalName(externalName());

    newDt = ios.resolveInputSymbol(newExternalName,d_dataType);
    d_dataType.restrict(newDt);

    setResolvedScriptOutput(newExternalName);

  } catch(const com::Exception& e) {
    // if file not found, etc..., other file
    throwAtFirst(e);
  } catch(const VSClash& ) {
    // set wrong dataType and re-throw
    d_dataType= DataType(newDt.vs(),d_dataType.st());
    throw;
  }
}



calc::DataType& calc::ASTSymbolInfo::dataType()
{
  return d_dataType;
}

VS calc::ASTSymbolInfo::vs() const
{
  return d_dataType.vs();
}

calc::OVS calc::ASTSymbolInfo::ovs() const
{
  return d_dataType.vs();
}

const calc::DataType&  calc::ASTSymbolInfo::dataType()  const
{
  return d_dataType;
}

void calc::ASTSymbolInfo::setFirstIsCreation()
{
  PRECOND(d_firstCodePresence);
  d_ioType.setInput(pcrxml::ModelInputType::None);
}

bool calc::ASTSymbolInfo::firstIsCreation() const
{
 PRECOND(d_firstCodePresence);
 return d_ioType.input() == pcrxml::ModelInputType::None;
}


//! set that the symbol is reported
/*! \param reportPar lhs of ASTAss, where reported
 *  \param report the report
 *  \param inDynamic is reportPar a node from the DynamicSection?
 *  \param xmlScriptInterface the model has XML only interface
 */
void calc::ASTSymbolInfo::setReport(const ASTPar *reportPar,
                                    const Report *report,
                                    bool inDynamic,
                                    bool xmlScriptInterface)
{
  PRECOND(reportPar);
  PRECOND(report);

  // only report from XML script if scriptOutput is present
  if (xmlScriptInterface)
    if (0 == d_definitionCreation ||
        !d_definition->scriptOutput().present())
            return;

  d_reportPar     =reportPar;
  d_report        =report;
  d_reportPosition=inDynamic ? RPDynamic : RPInitial;
}

void calc::ASTSymbolInfo::setObjectLinkConstructor(LinkInExpr *objectLinkConstructor)
{
  d_dataType.restrict(VS_OBJECT);
//  d_dataType=DataType(VS_OBJECT,ST_NON);
  d_objectLinkConstructor=objectLinkConstructor;
}

calc::LinkInExpr* calc::ASTSymbolInfo::objectLinkConstructor() const
{
  return d_objectLinkConstructor;
}

void calc::ASTSymbolInfo::setConstantByBinding(const ASTNumber *n)
{
  // check on lhsUse
  const ASTPar *f=d_firstAss;
  if (f) { // pcrcalc0
    std::ostringstream os;
    os << "Assigning value to constant binding: '"
       << name() << "=" << n->value() << "'";
    f->symError(os.str());
  }

  DataType newFT(dataType());
  try {
    newFT.restrict(n->returnDataType());
  } catch(const DataTypeClash& ) {
     // pcrcalc10d
     // set "wrong" type in symbol table
     // running BuildTypeVisitor again will
     // yield error
     d_dataType=n->returnDataType();
     throw;
  }

  resetDefinition(Binding);

  d_definition = new pcrxml::Definition(name());
  d_definition->field(pcrxml::FieldValueOrType());
  d_definition->field().get().number(n->value());


  d_dataType=newFT;
}

void calc::ASTSymbolInfo::setExternalNameByBinding(const std::string& externalName)
{
  resetDefinition(Binding);

  // from bindings: yet undecided if input or output
  //  TODO after d_ioType and setReport is done we know this
  //        and/or resolve

  d_definition = new pcrxml::Definition(name());
  d_definition->scriptInput(pcrxml::Exchange());
  d_definition->scriptInput().get().external(externalName);

  d_definition = new pcrxml::Definition(name());
  d_definition->scriptOutput(pcrxml::Exchange());
  d_definition->scriptOutput().get().external(externalName);
}


//! from interface
void calc::ASTSymbolInfo::setInfo(const ASTDefinition& ib)
{
  resetDefinition(Interface);
  d_definition = new pcrxml::Definition(name());

  if (!ib.description().empty())
   d_definition->description(pcrxml::Description(
        ib.description()));

  d_dataType.setUnit(ib.unit());

  d_hasInterface=true;
  d_definitionRole=ib.definitionRole();
}

void calc::ASTSymbolInfo::setResolvedScriptOutput(
    std::string const& foundFile)
{
  if (!d_definition) {
    // only found by RunDirectory and so on
    d_definition = new pcrxml::Definition(name());
    d_definitionCreation = Resolve;
  }
  if (!d_definition->scriptOutput().present())
   d_definition->scriptOutput(pcrxml::Exchange());
  d_definition->scriptOutput().get().external(foundFile);
}

#define X_IF_PRESENT_RETURN2(x1,x2,v)         \
 if (d_definition) {                          \
   if (d_definition->x1().present()){         \
    if (d_definition->x1()->x2().present()) { \
      return v;                               \
    }}}

#define X_IF_PRESENT_RETURN_GET2(x1,x2)        \
 X_IF_PRESENT_RETURN2(x1,x2, d_definition->x1()->x2().get())

//! 0 if symbol has no xml LookupTable
pcrxml::Relation const* calc::ASTSymbolInfo::lookupTable() const
{
  if (d_definition) {
   if (d_definition->relation().present()) {
    return &(d_definition->relation().get());
   }
  }
//  X_IF_PRESENT_RETURN2(relation,lookupTable,
//   &(d_definition->relation()->lookupTable().get()));
  return 0;
}

size_t calc::ASTSymbolInfo::memoryInputId() const
{
  X_IF_PRESENT_RETURN_GET2(scriptInput, memoryExchange);
  return noMemoryExchangeId();
}

size_t calc::ASTSymbolInfo::memoryOutputId() const
{
  X_IF_PRESENT_RETURN_GET2(scriptOutput, memoryExchange);
  return noMemoryExchangeId();
}

//! check if this is a constant
bool calc::ASTSymbolInfo::isConstant() const
{
  X_IF_PRESENT_RETURN2(field,number,true);
  return false;
}

//! get value of constant, 0 if not a constant
/*!
 *  always guard by isConstant() for meaning of 0
 */
double calc::ASTSymbolInfo::constantValue() const
{
  X_IF_PRESENT_RETURN_GET2(field,number);

  return 0;
}


std::string  calc::ASTSymbolInfo::externalName() const
{
  X_IF_PRESENT_RETURN_GET2(scriptInput,external);
  X_IF_PRESENT_RETURN_GET2(scriptOutput,external);

  return name();
}

//! empty string if not set
std::string  calc::ASTSymbolInfo::description() const
{
  if (d_definition)
    if (d_definition->description().present()) {
      return d_definition->description()->text();
    }
  return std::string();
}


size_t calc::ASTSymbolInfo::noMemoryExchangeId()
{
  return std::numeric_limits<size_t>::max();
}


//! set value of firstAss
void calc::ASTSymbolInfo::setFirstAss(const ASTPar *firstAss)
{
  if (!d_firstAss)
    d_firstAss=firstAss;
}


const std::string&  calc::ASTSymbolInfo::name() const
{
  PRECOND(d_firstCodePresence);
  PRECOND(!d_firstCodePresence->name().empty());
  return d_firstCodePresence->name();
}

//! only used on one spot in BuildTypesVisitor code
std::string calc::ASTSymbolInfo::firstText() const
{
  PRECOND(d_firstCodePresence);
  return d_firstCodePresence->position()->shortText();
}

//! get value of reportPosition
calc::ReportPosition calc::ASTSymbolInfo::reportPosition() const
{
  return d_reportPosition;
}

//! get value of report
const calc::Report* calc::ASTSymbolInfo::report() const
{
  return d_report;
}


//! the name presented to user in case of external failures
/*! default this is equal to the name(), except
 *  if the symbol has a binding, then it returns
 *    name() + "(binding=" + pathName() + ")"
 */
std::string calc::ASTSymbolInfo::errorMsgName() const
{
  if (name()==externalName())
    return name();
  return name() + "(binding=" + externalName() + ")";
}

//! rethrow the catched exception \a e for the position of d_firstCodePresence
void calc::ASTSymbolInfo::throwAtFirst(const com::Exception& e) const
{
  throw SymException(*(d_firstCodePresence->position()),
      errorMsgName(),e.messages());
}

//! (re-)throw SymException with the name of this in errorMsgName format
void calc::ASTSymbolInfo::throwSym(const SymException& s) const
{
  s.throwPos(errorMsgName());
}

void calc::ASTSymbolInfo::throwSym(
    const Position&      pos,
    const std::string& msg) const
{
  throw SymException(pos,errorMsgName(),msg);
}

/*!
 * \returns ASTPar node where symbol is written, 0 if not written
 */
const calc::ASTPar* calc::ASTSymbolInfo::reportPar() const
{
  return d_reportPar;
}

//! get value of d_stackInput
const calc::StackInput* calc::ASTSymbolInfo::stackInput() const
{
  return d_stackInput;
}

//! set value of d_ioType
void calc::ASTSymbolInfo::setIoType(const IOType& ioType)
{
  d_ioType=ioType;
}

//! get value of d_ioType
const calc::IOType& calc::ASTSymbolInfo::ioType() const
{
  return d_ioType;
}

/*
 * namespace calc {
 *   namespace Private {
 * 
 *   class MapDataType {
 *     DataType  d_dataType;
 *   public:
 *     MapDataType(const DataType& dataType):
 *       d_dataType(dataType) {}
 *     pcrxml::Spatial spatial() const {
 *       if (d_dataType.vs()==VS_MAPSTACK)
 *         return pcrxml::Spatial::Yes;
 * 
 *       switch(d_dataType.st()) {
 *         case ST_SPATIAL    : return pcrxml::Spatial::Yes;
 *         case ST_NONSPATIAL : return pcrxml::Spatial::Non;
 *         case ST_EITHER     : return pcrxml::Spatial::Either;
 *         case ST_NON        :
 *         default            : return pcrxml::Spatial::NA;
 *                              PRECOND(FALSE);
 *                      // ST_ERROR=0,
 *                      // ST_ALL = ST_EITHER|ST_NON
 *       }
 *     }
 *     pcrxml::DataTypeEnum dataTypeEnum() const {
 *       VS vs=d_dataType.vs();
 *       switch(vs) {
 *         case VS_TSS:   return pcrxml::DataTypeEnum::Tss;
 *         case VS_TABLE: return pcrxml::DataTypeEnum::Table;
 *         case VS_MAPSTACK:
 *                        vs= d_dataType.resultType();
 *                        // fall through;
 *         default:       if (isField(vs) && nrInSet(vs) > 1)
 *                          return pcrxml::DataTypeEnum::Field;
 *                        return pcrxml::pcrVs2DataType(vs);
 *      }
 *     }
 *     bool isExchangeItem() const {
 *      //
 *      //switch(d_dataType.st()) {
 *      // case ST_SPATIAL    :
 *      // case ST_NONSPATIAL : break;
 *      // default            : return d_dataType.vs()==VS_MAPSTACK;
 *      //}
 *        return true;
 *     }
 *   };
 *  }
 * }
*/


//! create a new Definition from info
/*!
 * \todo
 *   never returns 0, so better to return a value instead of a ptr
 */
pcrxml::Definition* calc::ASTSymbolInfo::createDefinition() const
{
  pcrxml::Definition *d(0);
  if (d_definition) {
   d = new pcrxml::Definition(*d_definition);
  } else {
    // no present, build
    d = new pcrxml::Definition(name());
  }

  // add elements needed if not yet there
  if (d_dataType.isField()) {
    if(!d->field().present())
         d->field(pcrxml::FieldValueOrType());
  } else {
    if (d_dataType.vs() == VS_TABLE)
      if(!d->relation().present())
         d->relation(pcrxml::Relation());
  }

  // attach field types
  if (d_dataType.isField()) {
    PRECOND(d->field().present());
    d->field()->dataType().set(toXMLDataType(d_dataType));
    if (!isConstant())
      d->field()->spatialType().set(st2XML(d_dataType.st()));
    d->modelExchange() = pcrxml::ModelExchange();
    d->modelExchange()->input(d_ioType.input());
    d->modelExchange()->output(d_ioType.output());
  }


  return d;

/*
 * Private::MapDataType      mdt(d_dataType);
 * pcrxml::ExchangeItem e;
 * e.variable              = new pcrxml::Variable();
 * e.variable->id          = name();
 * e.variable->spatial     = mdt.spatial();
 * pcrxml::DataTypeDTD dt;
 * dt.dimension            = d_dataType.unit().createXMLDimension();
 * dt.value                = mdt.dataTypeEnum();
 * e.variable->dataTypeDTD.push_back(new pcrxml::DataTypeDTD(dt));
 * e.variable->input       = d_ioType.input();
 * e.variable->output      = d_ioType.output();
 *
 * switch(d_definitionRole) {
 *    case Constant: // explicit in definition
 *        return 0;
 *    case Input: // explicit in definition
 *        e.exchangeDirection= pcrxml::ExchangeDirection::Input;
 *        break;
 *    case Output: // explicit in definition
 *        e.exchangeDirection= pcrxml::ExchangeDirection::Output;
 *        break;
 *    default: {
 *       // implicit filter out internal parameters
 *       //   no input
 *       //   not reported (as output)
 *       // ESRI Demo May 2005
 *       // MemoryIO
 *       switch(e.variable->input()) {
 *         case pcrxml::ModelInputType::None:
 *            if (!report()) // filter out
 *              return 0;
 *            else
 *              e.exchangeDirection= pcrxml::ExchangeDirection::Output;
 *            break;
 *         case pcrxml::ModelInputType::Dynamic:
 *            if (isField(vs()))
 *               e.variable->input =pcrxml::ModelInputType::Initial;
 *            // fall through
 *         default:
 *              e.exchangeDirection= pcrxml::ExchangeDirection::Input;
 *            ;
 *       }
 *       POSTCOND(e.variable->input() == pcrxml::ModelInputType::None ||
 *                e.variable->output()== pcrxml::ModelOutputType::Fixed);
 *     }
 * }
 * return new pcrxml::ExchangeItem(e);
 */
}

bool calc::ASTSymbolInfo::operator<(const ASTSymbolInfo& rhs)const
{
  return name() < rhs.name();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

std::ostream& calc::operator<<(std::ostream& s, calc::ASTSymbolInfo const& si)
{
  s << "name() (" << si.name() << ")" << std::endl;
  s << "d_dataType(" << si.dataType() << ")" << std::endl;
  if (si.d_definition) {
   std::ostringstream str;
   pcrxml::definition(str,*(si.d_definition),pcrxsd::namespaceInfoMap("PCRaster.xsd"));
   s << "d_definitionCreation(" << si.str(si.d_definitionCreation) << ")" 
     << std::endl;
   s << "d_definition(" << str.str() << ")" << std::endl;
  }
  return s;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



