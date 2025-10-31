#ifndef INCLUDED_COM_LEGEND
#define INCLUDED_COM_LEGEND

#include "stddefx.h"
#include "com_legendclass.h"

#include <string>
#include <vector>



namespace com {



//!  short_description
/*!
  longer_description
*/
template<class T>
class Legend
{

private:

  //! Title of the legend.
  std::string      d_title;

  //! Classes of the legend.
  std::vector<com_LegendClass<T> > d_classes;

public:

  typedef typename std::vector<com_LegendClass<T> >::iterator iterator;
  typedef typename std::vector<com_LegendClass<T> >::const_iterator
         const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Legend              ();

  //! Constructor.
                   Legend              (const std::string &t);

  //! Constructor.
                   Legend         (const std::vector<com_LegendClass<T> > &c);

  //! Constructor.
                   Legend              (const std::string &t,
                                   const std::vector<com_LegendClass<T> > &c);

  //! Destructor.
  /* virtual */    ~Legend             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the title of the legend to \a t.
  void             setTitle            (const std::string &t);

  //! Sets the classes to \a c.
  void             setClasses       (const std::vector<com_LegendClass<T> > &c);

  //! Resizes the classes datastructor to \a n classes.
  void             setNrClasses        (size_t n);

  //! Insert class \a c before position \a p to the legend.
  void             insert              (iterator p,
                                        T        c);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the title of the legend.
  const std::string &title             () const;

  //! Returns the classes.
  const std::vector<com_LegendClass<T> > &classes() const;

  iterator         begin               ();

  iterator         end                 ();

  const_iterator   begin               () const;

  const_iterator   end                 () const;

  //! Returns true if class \a c is in the legend.
  bool             binary_search       (T c) const;

  //! Returns the position where class \a c should be inserted.
  iterator         lower_bound         (T c);

  //! Returns the number of classes in the legend.
  size_t           nrClasses           () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace com

#endif
