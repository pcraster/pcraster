#ifndef INCLUDED_CALC_FIELDTYPE
#define INCLUDED_CALC_FIELDTYPE

#ifndef INCLUDED_CALCTYPES
#include "calctypes.h"   // VS and ST
#define INCLUDED_CALCTYPES
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace calc {

//! class is bollocsk, type description of a pcrcalc data item
/*!the type hierarchy of data items is:
   <UL>
   <LI><B>type</B>
       <UL>
        <LI>Field (being boolean,scalar,noninal,ordinal,ldd or directional)
  <UL>
   <LI>Spatial (map)</LI>
   <LI>NonSpatial (number)</LI>
  </UL>
        <LI>Table 
    <UL><LI>having a sub <B>type</B></LI></UL>
    If table is used in a lookup... operation the operation returns
    a result of type Field (Spatial is one of the arguments is spatial,
    NonSpatial otherwise).<BR>
    The new extended version can return actually any type, but the only
    subtype needed so far is <I>Table</I>.
  </LI>
        <LI>Tss (short for timeseries)
    <UL><LI>having a sub <B>type</B><BR>
    The only subtypes currently used is Field, with 3 possible occurences:
    <OL><LI>Tss NonSpatial<BR>
            A timeseries with 1 column</LI>
        <LI>Tss Spatial<BR>
            A.k.a a map stack</LI>
        <LI>Tss Multiple Columns<BR>
            In combination with a classified map,it transforms a multiple
      column tss to map stack (timeinput)<BR>
            In combination with a classified map,it transforms a map
      stack to a multiple column tss (timeoutput)
        </LI>

    </OL>
   </UL>
  </LI>
        <LI>Array
    <UL><LI>having a sub <B>type</B></LI></UL>
  </LI>
        <LI>ArrayIndex</LI>
  <LI>String (External hook)</LI>
       </UL>
    </LI>
    </UL>

    <P>
    Thus, some types have <B>sub</B> types, all others are <B>end</B> types.<BR>
    All types are dynamically build during the parsing and type building phases. This happens
    by repetive restrictment. On each involvement of the data item in these phases, more is known
    from it's context.
*/

class SyntaxArgumentError {
 public:
  std::string d_s;
  SyntaxArgumentError(const std::string& s);
};

class SyntaxVsClash {
  public:
  std::string d_oldVs,d_newVs;
  SyntaxVsClash(
    const std::string& oldVs, const std::string& newVs);
};

class SyntaxStClash {
  public: 
    std::string d_oldSt,d_newSt;
    SyntaxStClash(const std::string& oldSt, const std::string& newSt);
};


class InfoScript;
class Operator;

//! a Spatial or NonSpatial
/*! 
    Note that we can not subclass for spatial and non-spatial
    since a type can move from non-spatial to spatial
    <PRE> initial  a=0 dynamic a = spread(.....)</PRE>
    <BR>How to choose between spatial (S) and non-spatial (NS)?
    <PRE>
    - promotion from NS to S is possible while building script
    - an operation can require NS (only foreach ascendingby), thus forbid S
      ; record that point in script, since this error shows up later
    - if promotion happend when building, the types have to build again
      --> a second type building phase
    </PRE>
*/
class FieldType {
 private:
  //! current set of possible vs's
  VS       d_vs;
  //! spatial states possible
  const ST d_stType;
  //! is field spatial?
  bool  d_spatial;
 protected:
  VS setVs(VS newVs);
 public:
  // CREATORS
  //! st must be ST_SPATIAL,ST_NONSPATIAL (= fixed) or ST_DERIVED
  FieldType(VS vs, ST st);
  //! construct for for an expr-node
  FieldType(const Operator& o);
  // MANIPULATORS
  void restrictSystem(VS vsNewPossible, bool spatialByArgs);
  void restrictArg(const Operator& o, int argNr, int offsetArg) 
    /* THROW (SyntaxArgumentErrorExcep)*/;
  bool restrictUser(VS vsNewPossible, bool spatialByAssignment)
    /* THROW (SyntaxVsClashErrorExcep,SyntaxStClashErrorExcep)*/;
  // ACCESSORS
  bool spatialDerived() const;
  bool spatial() const;
   VS   vs() const;
  void print(InfoScript& i, const Operator& o) const;
  void print(InfoScript& i, const std::string& tag) const;
  void print(InfoScript& i) const;
};

}

#endif
