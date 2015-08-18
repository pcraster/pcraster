#ifndef INCLUDED_COM_PALETTE
#define INCLUDED_COM_PALETTE



#include <cstring>
#include <vector>
#include "com_colour.h"



/*!
  \class com_Palette
  \brief The com_Palette class is for palette objects which contain the
         the rgb colour definitions of a range of colours.

  A colour palette can be used by, for example, the layer classes to draw the
  geographical features.
*/
class com_Palette
{

private:

  //! The colours belonging to the palette.
  std::vector<com_Colour> d_colourRange;

  //! Assignment operator. NOT IMPLEMENTED.
  com_Palette &operator=               (const com_Palette &palette);

  //! Copy constructor. NOT IMPLEMENTED.
                   com_Palette         (const com_Palette &palette);

public:

  typedef std::vector<com_Colour>::const_iterator const_iterator;
  typedef std::vector<com_Colour>::iterator iterator;
  typedef std::vector<com_Colour>::const_reverse_iterator
                                   const_reverse_iterator;
  typedef std::vector<com_Colour>::reverse_iterator reverse_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   com_Palette         (const int colourRange[][3],
                                        int       colourMaxVal,
                                        size_t    nrColours);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Scales the rgb values of the colour members.
  void             scale               (double factor);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns an iterator to the first colour or end().
  const_iterator   begin               () const;

  //! Returns an iterator to the one-past-the-last colour.
  const_iterator   end                 () const;

  //! Returns a reverse iterator to the first colour or rend()
  const_reverse_iterator rbegin        () const;

  //! Returns a reverse iterator to the one-past-the-last colour.
  const_reverse_iterator rend          () const;

  //! Returns an iterator to the first colour or end().
  iterator         begin               ();

  //! Returns an iterator to the one-past-the-last colour.
  iterator         end                 ();

  //! Returns a reverse iterator to the first colour or rend()
  reverse_iterator rbegin              ();

  //! Returns a reverse iterator to the one-past-the-last colour.
  reverse_iterator rend                ();

  //! Returns the number of colours in the palette.
  size_t           getNrOfColours      () const;

};

#endif

