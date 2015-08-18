#ifndef INCLUDED_GEOM_PRDIMTREE
#define INCLUDED_GEOM_PRDIMTREE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_SQUARE
#include "geo_square.h"
#define INCLUDED_GEO_SQUARE
#endif
#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif

// Module headers.
#ifndef INCLUDED_GEOM_PRDIMTREESTATS
#include "geom_prdimtreestats.h"
#define INCLUDED_GEOM_PRDIMTREESTATS
#endif



namespace geom {
  // PRDimTree declarations.
  class ProximitySearch;
}

namespace geom {


 template <typename K>
  struct ElementIsKey {
    template <typename E>
      static const K& key(const E& e) {
        return e;
      }
    template <typename E>
      static void print(const E& ) {
      }
  };
 /*! the element has a ptr to the key method
  */
 template <typename K>
  struct ElementIsKeyPtr {
    template <typename E>
      static const K& key(const E& e) {
        return e->key();
      }
    template <typename E>
      static void print(const E& ) {
        // DEBUG STUFF 
      }
  };
 template <typename K>
  struct ElementHasKey {
    template <typename E>
      static const K& key(const E& e) {
        return e.key();
      }
    template <typename E>
      static void print(const E& ) {
      }
  };

template <typename T,
          size_t Dim,
          typename element,
          template<typename> class keyAccess =  ElementIsKey,
          typename bucketType = std::vector<element>
          >
  struct PRDimT {
    typedef T                   Coordinate;
    typedef T                   Distance;
    typedef geo::Point<T,Dim>   Point;     // actually Key
    typedef element             Element;
    typedef geo::Square<T,Dim,geo::ClosedOpenBoundaries>  Region;
    typedef keyAccess<Point>    KeyAccess;
    typedef bucketType          Bucket;
    static const size_t         dim=Dim;
  };

#define GEOM_PRDIMTREE_TYPES \
  typedef typename  T::Coordinate Coordinate; \
  typedef typename  T::Distance   Distance; \
  typedef typename  T::Element    Element; \
  typedef typename  T::Point      Point; \
  typedef typename  T::Region     Region; \
  typedef typename  T::KeyAccess  KeyAccess; \
  typedef typename  T::Bucket     Bucket

template<typename T> class PRDimTree;

/*!
 * \todo
 *  insert runtime metrics, such as nr of nodes visited to make
 *  formal tests for on-boundary point tests
 */
template <typename T>
 class PRDimNode
{
 private:
  GEOM_PRDIMTREE_TYPES;
  /*!
   * \todo
   *  heeft LeafNode wel een region nodig?
   */
 protected:
  const Region        d_region;
  const PRDimTree<T> *d_partOf;
 public:
    PRDimNode(const Region& region,
              const class PRDimTree<T> *partOf):
      d_region(region),d_partOf(partOf) {};

   virtual ~PRDimNode() {};
  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  virtual PRDimNode<T> *insert(const Element& p)=0;


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const Region& region() const {
    return d_region;
  }
  const Point& centre() const {
    return d_region.centre();
  }
  const Point& key(const Element& e) const {
    return KeyAccess::key(e);
  };
  virtual size_t count(const Point& p)const=0;
  virtual void proximitySearch(
        std::vector<Element>& result,
        const Point& p, const ProximitySearch& ps) const=0;
  virtual PRDimTreeStats  stats() const=0;
};

template <typename T>
 class PRInternalNode : public PRDimNode<T>
{
  GEOM_PRDIMTREE_TYPES;
  std::vector<PRDimNode<T> * > d_son;
 public:
  PRInternalNode(const Region& region,const PRDimTree<T> *partOf);
  virtual ~PRInternalNode();
  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  PRDimNode<T> *insert(const Element& p);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t count(const Point& p)const;
  void proximitySearch(
        std::vector<Element>& result,
        const Point& p, const ProximitySearch& ps) const;
  PRDimTreeStats  stats() const;
};

/*!
 * \todo
 *  calculate sizeof(Leaf)+sizeof(std::vector) as parameter
 *  for bucket-size overhead in mem. req. terms
 * \todo
 *  fully optimized: alleen offset/size in d_partOf owned
 *  array, Node slechts 8 bytes, parent moet region bijhouden.
 */
template <typename T>
 class PRLeafNode : public PRDimNode<T>
{
  GEOM_PRDIMTREE_TYPES;
//  typedef std::vector<Element>   Bucket;
  typedef typename Bucket::const_iterator BucketIter;

  Bucket d_bucket;

  /*! check needed while inserting, wether all points
   *  are equal and a split would lead to infinite splitting.
   * This bit can be put into the d_partOf ptr;
   * or could even be removed in a read-only tree
   */
  bool d_allEqual;

  public:
          PRLeafNode(const Region& region,
                     const PRDimTree<T> *partOf);
  virtual ~PRLeafNode();
  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  PRDimNode<T> *insert(const Element& p);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t count(const typename T::Point& p)const;
  void proximitySearch(
        std::vector<Element>& result,
        const Point& p, const ProximitySearch& ps) const;
  PRDimTreeStats  stats() const;
};

//! Base class (Template) for bucket-PR quad- oct- etc- trees.
/*!
   Implements datastructures known as a quadtree for the case Dim=2 and 
   octtree if Dim=3. T is the PRDimT settings

   Code derived from Quadtree books of H.Samet. Nomenclature as
   much as in these books.


*/
template <typename T>
 class PRDimTree
{
private:

  //! Assignment operator. NOT IMPLEMENTED.
  PRDimTree&           operator=           (const PRDimTree&);

  //! Copy constructor. NOT IMPLEMENTED.
                   PRDimTree               (const PRDimTree&);

  PRDimNode<T>    *d_root;
  size_t           d_bucketSize;


  GEOM_PRDIMTREE_TYPES;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PRDimTree               (const Point& centre,
                                            Coordinate  halfWidth,
                                            size_t      bucketSize);

  /* virtual */    ~PRDimTree              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  bool insert(const Element& p);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t           bucketSize          () const;

  size_t           count(const Point& p) const;
  void             proximitySearch(
                    std::vector<Element>& result,
                    const Point& p,
                    const ProximitySearch& ps) const;
  const Point&     centre()               const;

  PRDimTreeStats   stats() const;


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



} // namespace geom

#endif
