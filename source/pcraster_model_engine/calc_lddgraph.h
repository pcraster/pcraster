#ifndef INCLUDED_CALC_LDDGRAPH
#define INCLUDED_CALC_LDDGRAPH



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_DYNAMIC_BITSET
#include <boost/dynamic_bitset.hpp>
#define INCLUDED_BOOST_DYNAMIC_BITSET
#endif

// PCRaster library headers.
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
// Module headers.
#ifndef INCLUDED_CALC_ICACHEDOBJECT
#include "calc_icachedobject.h"
#define INCLUDED_CALC_ICACHEDOBJECT
#endif
#ifndef INCLUDED_CALC_RASTERGRAPH
#include "calc_rastergraph.h"
#define INCLUDED_CALC_RASTERGRAPH
#endif



namespace calc {
  // LddGraph declarations.
}



namespace calc {



//! as ldd raster
/*!
 *  as ldd type, encode to what vertex (cell) the FieldId flows
 *
 *  The graph does not garantuee that each path ends in a pit.
 *
 *  \todo Rewrite this class to be smarter about the iterators. The current
 *        implementation might be correct, but the Visual Studio STL
 *        understandably assumes the iterators are invalid (in debug builds).
 *        This is because the d_edges container is updated while iterators
 *        are determined (in constructor taking the isMV argument).
 */
class LddGraph : public RasterGraph, public ICachedObject
{
public:
  struct Unsound: public com::Exception {
    Unsound():
     com::Exception("Unsound ldd")
     {}
  };

  //! a single edge from d_sourceVertex to d_targetVertex
  struct Edge {
    FieldId d_sourceVertex;
    FieldId d_targetVertex;
    FieldId source() const { return d_sourceVertex; }
    FieldId target() const { return d_targetVertex; }
    //! upstream
    FieldId up()     const { return d_sourceVertex; }
    //! downstream
    FieldId down()   const { return d_targetVertex; }
  };
  typedef std::vector<Edge>             Edges;
  typedef Edges::iterator               UpIterator;
  typedef Edges::const_iterator         UpConstIterator;

  typedef Edges::reverse_iterator       DownIterator;
  typedef Edges::const_reverse_iterator DownConstIterator;

  typedef size_t                        PitId;
  struct Catchment {
    //! field Id of a pit, index into d_pitFieldIds serves as PitId
    //! can be invalid if created by masked MV
    FieldId          d_pitId;
    Edges::iterator  d_beginEdge;
    Edges::iterator  d_endEdge;
    Catchment(PitId pitId):
      d_pitId(pitId)
     {}
    DownConstIterator downBegin() const {
     return DownConstIterator(d_endEdge);
    }
    DownConstIterator downEnd() const {
     return DownConstIterator(d_beginEdge);
    }
  };

  typedef std::vector<Catchment>        Catchments;
  typedef Catchments::const_iterator    CatchmentsConstIterator;



private:
  FieldId                 d_invalidFieldId;
  /*! order as such as that iterating is possible in both upstream and
   *  downstream order
   */
  Edges                    d_edge;

  Catchments               d_catchments;

  //X for each FieldId map to index in d_pitFieldIds for its downstream pit
  /*X
   * in the case of unsound ldd's or cuts by mv mask ctor a PitId
   * equal or larger to d_pitFieldIds.size() may be assigned.
   * disabled, not needed, test commented in
   * void calc::LddGraphTest::testFieldIdToPitId()
      std::vector<PitId>       d_fieldIdToPitId;
      void initPitIds();
   */

  void addEdge(FieldId sV, FieldId tV);


private:

  //! Assignment operator. NOT IMPLEMENTED.
  LddGraph&           operator=           (LddGraph const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   LddGraph               (LddGraph const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LddGraph               (
                       const UINT1 *lddField,
                       const IFieldRDConversion& conv);

                  LddGraph                (
                      const LddGraph& org,
                      const boost::dynamic_bitset<>& isMV,
                      bool  propagateDownstream);

  /* virtual */    ~LddGraph              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  UpConstIterator upBegin() const {
    return d_edge.begin();
  }
  UpConstIterator upEnd() const {
    return d_edge.end();
  }
  DownConstIterator downBegin() const {
    return d_edge.rbegin();
  }
  DownConstIterator downEnd() const {
    return d_edge.rend();
  }

  Catchments::const_iterator catchmentsBegin() const {
    return d_catchments.begin();
  }
  Catchments::const_iterator catchmentsEnd() const {
    return d_catchments.end();
  }

  bool invalid(FieldId fid) const {
    return fid == d_invalidFieldId;
  }


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



} // namespace calc

#endif
