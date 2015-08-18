#ifndef INCLUDED_COM_CLASSCLASSIFIER
#define INCLUDED_COM_CLASSCLASSIFIER



#include <set>
#include <vector>                  // cpp makes an error(?)
#include "com_legendclass.h"



//namespace pack {



/*!
  \class com_ClassClassifier
  \brief The com_ClassClassifier class is for objects holding classification
         properties of layers which hold classified data.
*/
template<class T>
class com_ClassClassifier
{

private:

  //! The lut for class numbers
  std::vector<com_LegendClass<T> > d_classes;

  //! Empties the d_classes member.
  void             clearClasses        ();

public:

  typedef typename std::vector<com_LegendClass<T> >::const_iterator
         const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
                   com_ClassClassifier ();

  //! Destructor.
                   ~com_ClassClassifier();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setClasses          (int first,
                                        int last);

  //! Copies the information about classnumbers from \a c.
  void             setClasses          (const std::vector<T> &c);

  //! Copies the information about classnumbers from \a c.
  void             setClasses          (const std::set<T> &c);

  //! Copies the information about the first \a n classes from \a c.
  void             setClasses          (const T *c,
                                        size_t   n);

  void             setClasses          (T const* values,
                                        std::string const* labels,
                                        size_t size);

  //! Copies the information about classnumbers from \a c.
  void             setClasses       (const std::vector<com_LegendClass<T> > &c);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the number of classes.
  size_t           nrClasses           () const;

  //! Returns the class index of class \a v.
  size_t           index               (T v) const;

  //! Returns the class value of class with index \a i.
  T                value               (size_t i) const;

  //! Returns the description of the class with index \a i.
  std::string      descr               (size_t i) const;

  //! Returns the class numbers.
  std::vector<T>   classNumbers        () const;

  //! Returns the legend classes.
  const std::vector<com_LegendClass<T> > &classes() const;

  const_iterator   begin               () const;

  const_iterator   end                 () const;

};



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------



//} // namespace pack

#endif
