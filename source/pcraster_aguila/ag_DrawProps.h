#ifndef INCLUDED_AG_DRAWPROPS
#define INCLUDED_AG_DRAWPROPS



#include <string>
#include <vector>
#include <QColor>



namespace com {
  class RawPalette;
}



namespace ag {



//! The DrawProps class contains the common information for draw subclasses.
/*!
*/
class DrawProps
{

private:

  static size_t    _freeColourId;

  static com::RawPalette const* _datasetColours;

  //! Legend title.
  std::string      _title;

  //! Colour assigned to this dataset.
  QColor           _colour;

  //! Palette for the colours.
  com::RawPalette const* _palette;

protected:

  //! Number of classes.
  size_t           _nrClasses;

  //! Colours for the classes.
  std::vector<QColor> _colours;

  //! Labels for the classes.
  std::vector<std::string> _labels;

protected:

  virtual void     reMapColours        () { } ;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DrawProps           (std::string const& title,
                                        com::RawPalette const* p);

                   DrawProps           (DrawProps const& properties);

  virtual          ~DrawProps          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  DrawProps&       operator=           (DrawProps const& rhs);

  bool             equals              (DrawProps const& rhs) const;

  void             setPalette          (const com::RawPalette* palette);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string& title             () const;

  const com::RawPalette* palette       () const;

  size_t           nrClasses           () const;

  QColor const&    colour              () const;

  const QColor&    colourByIndex       (size_t i) const;

  const std::string& label             (size_t i) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (DrawProps const& lhs,
                                        DrawProps const& rhs);

bool               operator!=          (DrawProps const& lhs,
                                        DrawProps const& rhs);

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
