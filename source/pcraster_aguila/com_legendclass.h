#ifndef INCLUDED_COM_LEGENDCLASS
#define INCLUDED_COM_LEGENDCLASS



#include <functional>
#include <string>



//namespace pack {



/*!
  \class com_LegendClass
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
template<class T>
class com_LegendClass
{

private:

  //! The class value.
  T                d_value;

  //! Description of the class value.
  std::string      d_descr;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
                   com_LegendClass     ();

  //! Constructor.
                   com_LegendClass     (T v);

  //! Constructor.
                   com_LegendClass     (T                  v,
                                        const std::string &d);

  //! Destructor.
                   ~com_LegendClass    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the value of the class.
  void             setValue            (T v);

  //! Sets the description of the class.
  void             setDescr            (const std::string &d);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the value of the class.
  T                value               () const;

  //! Returns the description of the class
  const std::string &descr             () const;

};



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------

// Function object.
template<class T>
struct compClass1: public std::binary_function<com_LegendClass<T>, T, bool>
{
  bool operator() (const com_LegendClass<T> &lc, T c) const
  { return lc.value() < c; }
};

template<class T>
struct compClass2: public std::binary_function<com_LegendClass<T>, T, bool>
{
  bool operator() (T c, const com_LegendClass<T> &lc) const
  { return c < lc.value(); }
};

template<class T>
struct compClass: public std::binary_function<com_LegendClass<T>, T, bool>
{
  bool operator() (const com_LegendClass<T> &lc, T c) const
  { return lc.value() < c; }

  bool operator() (T c, const com_LegendClass<T> &lc) const
  { return c < lc.value(); }
};

// Function object.
template<class T>
struct equalClass: public std::binary_function<com_LegendClass<T>, T, bool>
{
  bool operator() (const com_LegendClass<T> &lc, T c) const
  { return lc.value() == c; }

  bool operator() (T c, const com_LegendClass<T> &lc) const
  { return c == lc.value(); }
};



//} // namespace pack

#endif
