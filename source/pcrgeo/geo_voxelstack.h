#ifndef INCLUDED_GEO_VOXELSTACK
#define INCLUDED_GEO_VOXELSTACK



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif

#ifndef INCLUDED_GEO_VOXEL
#include "geo_voxel.h"
#define INCLUDED_GEO_VOXEL
#endif



namespace geo {



/*!
  \class VoxelStack
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class VoxelStack
{

private:

  //! Z-coordinate of the base of the column of voxels.
  REAL8            d_bottom;

  //! Column of voxels.
  std::vector<Voxel> d_column;

  //! Frees dynamically allocated memory.
  void             clean               ();

public:

  typedef std::vector<Voxel>::const_iterator const_iterator;
  typedef std::vector<Voxel>::const_reverse_iterator const_reverse_iterator;
  typedef std::vector<Voxel>::iterator iterator;
  typedef std::vector<Voxel>::reverse_iterator reverse_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   VoxelStack          ();

  //! Copy constructor.
                   VoxelStack          (const VoxelStack& vs);

  //! Destructor.
  /* virtual */    ~VoxelStack         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  VoxelStack &     operator=           (const VoxelStack& vs);

  //! Sets the z-coordinate of the base of the column of voxels.
  void             setBottom           (REAL8 b);

  //! Adds \a n voxels w sed type \a s and thickness \a t to top of the stack.
  void             add                 (size_t n,
                                        INT4 s,
                                        REAL8 t);

  //! Adds \a n copies of voxel \a v to the top of the stack.
  void             add                 (size_t n,
                                        const Voxel &v);

  //! Erases the voxels between iterator positions [\a b, \a e>.
  void             erase               (iterator b,
                                        iterator e);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns an iterator to the first (lowest) voxel.
  iterator         begin               ();

  //! Returns a const iterator to the first (lowest) voxel.
  const_iterator   begin               () const;

  //! Returns an reverse iterator to the first (lowest) voxel.
  reverse_iterator rbegin              ();

  //! Returns a const reverse iterator to the first (lowest) voxel.
  const_reverse_iterator rbegin        () const;

  //! Returns an iterator to the one-past-the-last (highest) voxel.
  iterator         end                 ();

  //! Returns a const iterator to the one-past-the-last (highest) voxel.
  const_iterator   end                 () const;

  //! Returns an reverse iterator to the one-past-the-last (highest) voxel.
  reverse_iterator rend                ();

  //! Returns a const reverse iterator to the one-past-the-last (highest) voxel.
  const_reverse_iterator rend          () const;

  //! Returns a const reference to the top voxel.
  const Voxel &    topVoxel            () const;

  //! Returns a reference to the top voxel.
  Voxel &          topVoxel            ();

  //! Returns a const reference to the bottom voxel.
  const Voxel &    bottomVoxel         () const;

  //! Returns a reference to the bottom voxel.
  Voxel &          bottomVoxel         ();

  //! Returns the z-coordinate of the top of the highest voxel.
  REAL8            top                 () const;

  //! Returns the z-coordinate of the bottom of the lowest voxel.
  REAL8            bottom              () const;

  //! Returns the thickness of the stack.
  REAL8            thickness           () const;

  //! Returns the number of voxels in the stack.
  size_t           nrVoxels            () const;

  //! Returns true if the stack contains no voxels.
  bool             empty               () const;

  //! Returns a const reference to the voxel at index position \a i.
  const Voxel &    operator[]          (size_t i) const;

  //! Returns a reference to the voxel at index position \a i.
  Voxel &          operator[]          (size_t i);

  //! Returns the depth at which voxel pointed by \a it is buried.
  REAL8            depth               (const_iterator it) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

inline void geo::VoxelStack::setBottom(REAL8 b)
{ d_bottom = b; }

inline void geo::VoxelStack::add(size_t n, INT4 s, REAL8 t)
{
  Voxel v(s, t);
  d_column.insert(d_column.end(), n, v);
}

inline void geo::VoxelStack::add(size_t n, const Voxel &v)
{ d_column.insert(d_column.end(), n, v); }

inline void geo::VoxelStack::erase(iterator b, iterator e)
{ d_column.erase(b, e); }

inline geo::VoxelStack::iterator geo::VoxelStack::begin()
{ return d_column.begin(); }

inline geo::VoxelStack::const_iterator geo::VoxelStack::begin() const
{ return d_column.begin(); }

inline geo::VoxelStack::reverse_iterator geo::VoxelStack::rbegin()
{ return d_column.rbegin(); }

inline geo::VoxelStack::const_reverse_iterator geo::VoxelStack::rbegin() const
{ return d_column.rbegin(); }

inline geo::VoxelStack::iterator geo::VoxelStack::end()
{ return d_column.end(); }

inline geo::VoxelStack::const_iterator geo::VoxelStack::end() const
{ return d_column.end(); }

inline geo::VoxelStack::reverse_iterator geo::VoxelStack::rend()
{ return d_column.rend(); }

inline geo::VoxelStack::const_reverse_iterator geo::VoxelStack::rend() const
{ return d_column.rend(); }

inline REAL8 geo::VoxelStack::top() const
{
  REAL8 b = d_bottom;

  for(const_iterator it = begin(); it != end(); it++)
    b += (*it).thickness();

  return b;
}

inline REAL8 geo::VoxelStack::bottom() const
{ return d_bottom; }

inline REAL8 geo::VoxelStack::thickness() const
{ return top() - bottom(); }

inline size_t geo::VoxelStack::nrVoxels() const
{ return d_column.size(); }

inline bool geo::VoxelStack::empty() const
{ return nrVoxels() == 0; }

inline const geo::Voxel &geo::VoxelStack::operator[](size_t i) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(i < d_column.size());
#endif

  return d_column[i];
}

inline geo::Voxel &geo::VoxelStack::operator[](size_t i)
{
#ifdef DEBUG_DEVELOP
  PRECOND(i < d_column.size());
#endif

  return d_column[i];
}

inline const geo::Voxel &geo::VoxelStack::topVoxel() const
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_column.size() > 0);
#endif

  return d_column.back();
}

inline geo::Voxel &geo::VoxelStack::topVoxel()
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_column.size() > 0);
#endif

  return d_column.back();
}

inline const geo::Voxel &geo::VoxelStack::bottomVoxel() const
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_column.size() > 0);
#endif

  return d_column.front();
}

inline geo::Voxel &geo::VoxelStack::bottomVoxel()
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_column.size() > 0);
#endif

  return d_column.front();
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace geo

#endif
