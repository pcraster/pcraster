#ifndef INCLUDED_PALETTE
#define INCLUDED_PALETTE



// External headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_QCOLOR
#include <QColor>
#define INCLUDED_QCOLOR
#endif

// Project headers.

// Module headers.



namespace ag {
  // Palette declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class Palette: public std::vector<QColor>
{

  friend class PaletteTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Palette             ();

  template<class InputIterator>
                   Palette             (InputIterator begin,
                                        InputIterator end);

  /* virtual */    ~Palette            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class InputIterator>
inline Palette::Palette(
         InputIterator begin,
         InputIterator end)
  : std::vector<QColor>(begin, end)
{
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

#endif
