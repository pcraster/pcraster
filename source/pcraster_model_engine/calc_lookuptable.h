#ifndef INCLUDED_CALC_LOOKUPTABLE
#define INCLUDED_CALC_LOOKUPTABLE

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif
#ifndef INCLUDED_CALC_RELATIONRECORD
#include "calc_relationrecord.h"
#define INCLUDED_CALC_RELATIONRECORD
#endif

struct LOOK_UP_TABLE;
struct LOOK_UP_KEY;

namespace com {
 template<typename T> class Interval;
}

namespace pcrxml {
 class Relation;
}

namespace calc {

class ASTSymbolInfo;
namespace detail {
struct LookupTablePrefixMap;
class MemoryInputTableCreator;
}

//! a lookup table as used by the lookup...() functions
/*!


  \todo
  table lookup failure, mischien in alle gevallen een RuntimeError?
  - runtime error YES (DomainError), nu in DynamicWave
  - pak de min en/of max, MWF ?
  - MV, nu in lookupscalar,lookupVS funcs

 \todo
   voeg sequence nr toe aan keys, sorteer keys incl.
   seq.nr, dan lower_bound or equal_range: Orde van
   voorkomen in table is dan meegenomen.

\par Generic Table Concept

 \todo praat met Edzer of het de S equivalent van lookuptables.

<ol>
<li> lookup...(e.g. scalar) \n
 key+   -> result \n
 een combinatie van 1 or meer keys moeten exact matchen
 anders MV, result is altijd laatste kolom, tabel wordt afgelopen tot 1 match is gevonden.
</li>
<li>uitvoer = lookup linear(v.tbl, invoer); \n
 1 scalar key  -> result \n
 tabel is geordend op key waarde, als geen exacte match interpoleer
 lager en hogere waarde. als begin en eind record niet tot (-)oneindig
 gaan dan wordt er een MV teruggeven bij zoeken buiten range.
 Voorbeeld:
 \code
 <   , 3 ]   0
 4           1
 6           1
 < 7 , 8 ]   0
 \endcode
 Deze tabel geeft de volgende invoer/uitvoer relatie:

 \code
 1   -> 0
 3.3 -> 0.3
 3.6 -> 0.6
 4   -> 1
 5   -> 1
 6   -> 1
 6.4 -> 0.6
 7.9 -> 0
 9   -> MV
 \endcode
 </li><li>
multi-way-function tabel voor ID -> (H<->A<->P)
  \verbatim
    a=lookupmwf????("v.tbl:h,a,p",..........)
     v.tbl:h,a,p
      read table v.tbl with (h<->a<->p) relation
      and name column 1 h, 2 a and column 3 p

    a=lookupmwf????("v.tbl:*,h,a,p",..........)
     v.tbl:*,h,a,p
     * prefixed by normal ID lookup (e.g SegmentID)

    get A given H for SegmentID:
    H = expr;
    A =lookupmwf???("v.tbl:*,h,a,p->a",SegmentID,h:H);
     v.tbl:*,h,a,p->a
     zoals boven, -> yielding column a
  \endverbatim
</li>
</ol>

 - Of met column-nrs (kijk ook hoe ODBC-ascii driver dit doet)
 -  implemteer het via ODBC en doe alleen column nrs / ascii driver als freebie
 - flikker lookupVS weg en maak lookup(  ) die via conversion(lookup(......))
    een type krijgt indien nodig.
 - default selection criteria voor
   \code
   lookup(table,expr1,...,exprN) is  \n
     lookup(table:1,...,N->"N+1",expr1,...,exprN);  \n
     MWF ?  -> of ander symbool \n
     lookup(table:1,...,N->?->"N+1",expr1,...,exprN); \n
   \endcode

Mischien handig over de mwf tabel  nog te kunnen zeggen of
de laagste en hoogste samen een interval vormen (daarbuiten MV) of
dat impliciet een      \n
  <   ,  x1]   y1  z1  \n
voor 1e regel komt:    \n
         x1    y1  z1  \n
en/of                  \n
  [ xN ,   >   yN  zN  \n
voor laatste regel komt: \n
         xN    yN  zN  \n

Generic Notation: [KeySelection]* -> (MWF|Linear|value)
<dl>
<dt>KeySelection</dt><dd>type 1) lookup key-columns, with value as result</dd>
<dt>Linear</dt><dd>collectie (x,value) pairs bereken uit x een value
            met x increasing is dus MWF A->value</dd>
<dt>MWF</dt><dd>collectie (A,...,X,Y,Z) tuples, met alle columns
            increasing values.</dd>
</dl>

 */
class LookupTable : public DataValue {
public:
   typedef RelationRecord::Key                  Key;
   typedef std::vector<RelationRecord>          Records;
   typedef Records::const_iterator              const_iterator;
private:
   detail::LookupTablePrefixMap                 *d_prefixMap;
   detail::MemoryInputTableCreator              *d_memoryInputTableCreator;
   //! vs of each column
   std::vector<VS>                               d_vs;

   typedef Records::const_iterator               I;

   Records                                       d_records;

   LOOK_UP_TABLE          *createOldStyle(const std::string &fileName);
   void                    reset         (const std::vector<VS>& vs);

   //! not implemented
   LookupTable(LookupTable const& table);

  void                     setXMLRecords        (pcrxml::Relation const& r,
                                                 const std::vector<VS>& vs);

  Records::const_iterator find    (const Key& prefixKey) const;

  bool interpolate(double& result,
                   const_iterator  begin,  const_iterator end,
                   double  keyValue,
                   size_t  keyCol,
                   size_t  resultCol) const;

  void                    clear();

 public:
  LookupTable(const ASTSymbolInfo& i);

  LookupTable();

  virtual ~LookupTable();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void                     setRecords           (const std::string&     fileName,
                                                 const std::vector<VS>& vs);

  void                     setRecords           (const Records& records,
                                                 const std::vector<VS>& vs);

  void                     setPrefixStableSort  (size_t prefixLen);

  void                     setArrayValue        (void const* data);
  DataValue*               load                 ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  OVS                      ovs                   () const;

  size_t                   nrCols                () const;

  bool                     find                  (double& result,
                                                  const Key& prefixKey) const;

  bool interpolate(double& result, const Key& keys) const;
  bool interpolate(double&    result,
                   const Key& prefixKey,
                   double     keyValue,
                   size_t     keyCol,
                   size_t     resultCol) const;
   Records  const& records() const;
};


}

#endif
