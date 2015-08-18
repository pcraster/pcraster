#ifndef INCLUDED_MLDD_GRAPH
#define INCLUDED_MLDD_GRAPH

#include <boost/config.hpp>
#include <boost/iterator_adaptors.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/properties.hpp>

#ifndef INCLUDED_MLDD_OUTEDGEITERATOR
#include "mldd_outedgeiterator.h"
#define INCLUDED_MLDD_OUTEDGEITERATOR
#endif
#ifndef INCLUDED_MLDD_VERTEXITERATOR
#include "mldd_vertexiterator.h"
#define INCLUDED_MLDD_VERTEXITERATOR
#endif

#ifndef INCLUDED_MLDD_DAGRASTER
#include "mldd_dagraster.h"
#define INCLUDED_MLDD_DAGRASTER
#endif

// adapted from boost/graph/mldd_graph.hpp
// this is complex stuff, some definitions seems to be needed
// in boost namespace others in mldd

namespace mldd {

  struct mldd_in_edge_iterator_policies
  {
    template <typename Iter>
    static void initialize(Iter& ) { }

    template <typename Iter>
    static void increment(Iter& i)
    { /* i.base() = Succ_Adj_Edge(i.base(), 1); */ }

    template <typename Iter>
    static void decrement(Iter& i)
    { /* i.base() = Pred_Adj_Edge(i.base(), 1); */ }

    template <typename Iter>
    static  Edge dereference(const Iter& i)
    { return i.edge(); }

    template <typename Iter>
    static bool equal(const Iter& x, const Iter& y)
    { return x.base() == y.base(); }
  };

  struct mldd_adjacency_iterator_policies
  {
    static void initialize(Vertex& ) { }

    template <typename Iter>
    static void increment(Iter& i)
    { /* i.base() = Succ_Adj_Edge(i.base(), 0); */ }

    template <typename Iter>
    static void decrement(Iter& i)
    { /* i.base() = Pred_Adj_Edge(i.base(), 0); */ }

    template <typename Iter>
    static Vertex dereference(const Iter& i)
    { return target(i.base()); }
    // gcc 3.3 CW { return ::target(i.base()); }

    template <typename Iter>
    static bool equal(const Iter& x, const Iter& y)
    { return x.base() == y.base(); }
  };


} // namespace mldd

#if !defined BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION
namespace boost {

  struct mldd_graph_traversal_category :
    public virtual boost::bidirectional_graph_tag,
    public virtual boost::adjacency_graph_tag,
    public virtual boost::vertex_list_graph_tag { };

  template <>
  struct graph_traits < mldd::DagRaster > {

   typedef mldd::Vertex           vertex_descriptor;
   typedef mldd::Edge             edge_descriptor;

   typedef mldd::OutEdgeIterator  out_edge_iterator;
   typedef mldd::VertexIterator   vertex_iterator;

    typedef boost::iterator_adaptor<mldd::Edge,
    //  mldd::mldd_adjacency_iterator_policies,
      mldd::Vertex, mldd::Vertex, const mldd::Vertex*,
      boost::multi_pass_input_iterator_tag,
      std::ptrdiff_t
    > adjacency_iterator;


    typedef boost::iterator_adaptor<mldd::Edge,
    //  mldd::mldd_in_edge_iterator_policies,
      mldd::Edge, const mldd::Edge&, const mldd::Edge*,
      std::forward_iterator_tag,
      std::ptrdiff_t
    > in_edge_iterator;


    typedef boost::directed_tag directed_category;
    typedef boost::allow_parallel_edge_tag edge_parallel_category; // not sure here
    typedef mldd_graph_traversal_category traversal_category;
    typedef size_t vertices_size_type;
    typedef size_t edges_size_type;
    typedef size_t degree_size_type;

    static inline vertex_descriptor null_vertex() {
      return mldd::Vertex(0u, 0u);
    }
  };

  /* Modification properties not used
  template <>
  struct vertex_property< mldd::DagRaster  > {
    typedef int type;
  };


  template <class vtype, class etype>
  struct edge_property< mldd::DagRaster  > {
    typedef etype type;
  };
  */

} // namespace boost
#endif

namespace mldd {

  typedef mldd::DagRaster G;

  boost::graph_traits<G>::vertex_descriptor
  source(boost::graph_traits<G>::edge_descriptor e,
         const G& /* g */)
  {
    return e.source();
  }

  boost::graph_traits<G>::vertex_descriptor
  target(boost::graph_traits<G>::edge_descriptor e,
         const G& /* g */)
  {
    return e.target();
  }

  std::pair<
    boost::graph_traits<G>::out_edge_iterator,
    boost::graph_traits<G>::out_edge_iterator >
  out_edges(
    boost::graph_traits<G>::vertex_descriptor u,
    const G& g)
  {
    typedef boost::graph_traits<G>::out_edge_iterator Iter;
    return std::make_pair( Iter(g.beginOutEdge(u)), Iter(g.endOutEdge(u)) );
  }

/*
// inline std::pair<
//   graph_traits<G>::in_edge_iterator,
//   graph_traits<G>::in_edge_iterator >
// in_edges(
//   graph_traits<G>::vertex_descriptor u,
//   const G& g)
// {
//   typedef graph_traits<G>::in_edge_iterator Iter;
//   return std::make_pair( Iter(First_Adj_Edge(u,1)), Iter(0) );
// }
// 
// inline std::pair<
//   graph_traits<G>::adjacency_iterator,
//   graph_traits<G>::adjacency_iterator >  
// adjacent_vertices(
//   graph_traits<G>::vertex_descriptor u, 
//   const G & g)
// {
//   typedef graph_traits<G>::adjacency_iterator Iter;
//   return std::make_pair( Iter(First_Adj_Edge(u,0)), Iter(0) );
// }
*/
   boost::graph_traits<G>::vertices_size_type
   num_vertices(const G & g)
  {
    return g.nrVertices();
  }

/*
//  graph_traits<G>::edges_size_type
//  num_edges(const G & g)
//  {
//    return g.nrEdges();
//  }
*/

  boost::graph_traits<G>::degree_size_type
  out_degree(
    boost::graph_traits<G>::vertex_descriptor u,
    const G &g)
  {
    return g.nrOutflowNB(u);
  }

  boost::graph_traits<G>::degree_size_type
  in_degree(
    boost::graph_traits<G>::vertex_descriptor u,
    const G &g)
  {
    return g.nrInflowNB(u);
  }

  boost::graph_traits<G>::degree_size_type
  degree(
    boost::graph_traits<G>::vertex_descriptor u,
    const G &g)
  {
    return in_degree(u,g) + out_degree(u,g);
  }
/*
// graph_traits<G>::vertex_descriptor
// add_vertex(G & g)
// {
//   return g.new_node();
// }
//
// graph_traits<G>::vertex_descriptor
// add_vertex(const vtype& vp, G & g)
// {
//   return g.new_node(vp);
// }
*/


  typedef DagRaster G;

  std::pair<
    boost::graph_traits<G>::vertex_iterator,
    boost::graph_traits<G>::vertex_iterator >
  vertices(const G& g)
  {
    typedef boost::graph_traits<G>::vertex_iterator Iter;
    return std::make_pair( Iter(g,g.beginVertex()), Iter(g,g.endVertex()) );
  }

  template < typename Val>
  struct  DagRasterPropertyMap
   : public boost::put_get_helper< Val, DagRasterPropertyMap<Val> >
  {
/*
   private:
    DagRasterPropertyMap():
      d_props(0)
      {};
 */
   public:

    geo::RasterDim    d_rd;
    std::vector<Val> &d_props;

    typedef boost::read_write_property_map_tag category;
    typedef Val                         value_type;
    typedef value_type&                 reference;
    typedef Vertex                      key_type;
    const reference operator[](const key_type& v) const
    {
      return d_props[d_rd.convert(v)];
    }
    reference operator[](const key_type& v)
    {
      return d_props[d_rd.convert(v)];
    }
/*
    //! Assignment operator.
    DagRasterPropertyMap& operator=(const DagRasterPropertyMap& rhs)
    {
      if (this != &rhs) {
        d_rd    =rhs.d_rd;

        delete [] d_props;
        d_props =new Val[d_rd.nrCells()];
        std::copy(rhs.d_props+0,
                  rhs.d_props+d_rd.nrCells(),
                  d_props+0);
      }
      std::cout << "proper =\n";
      return *this;
    }

    //! Copy constructor. NOT IMPLEMENTED.
    DagRasterPropertyMap(const DagRasterPropertyMap& rhs):
        d_rd(rhs.d_rd),
        d_props(new Val[d_rd.nrCells()])
    {
        std::copy(rhs.d_props+0,
                  rhs.d_props+d_rd.nrCells(),
                  d_props+0);
      std::cout << "proper copyctor\n";
    }
*/

    DagRasterPropertyMap(const geo::RasterDim& rd,
                         std::vector<Val> &props):
      d_rd(rd),
      d_props(props)
      {}
    ~DagRasterPropertyMap()
      {};
  };

} // namespace mldd


#endif // INCLUDED_MLDD_GRAPH
