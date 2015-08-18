#ifndef INCLUDED_COM_COLOUR
#define INCLUDED_COM_COLOUR





/*!
  \class com_Colour
  \brief The com_Colour class is for colours which are defined by an RGB
         colour value combination.
*/
class com_Colour
{

private:

  //! Value for red fraction of colour.
  int              d_red;

  //! Value for green fraction of colour.
  int              d_green;

  //! Value for blue fraction of colour.
  int              d_blue;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
                   com_Colour          ();

  //! Constructor.
                   com_Colour          (int red,
                                        int green,
                                        int blue);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Scales the colour values according to \a factor.
  void             scale               (double factor);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Return the red colour value.
  int              getRed              () const;

  //! Return the green colour value.
  int              getGreen            () const;

  //! Return the blue colour value.
  int              getBlue             () const;

};

#endif

