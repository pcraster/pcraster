#ifndef INCLUDED_AG_DATAPROPERTY
#define INCLUDED_AG_DATAPROPERTY



// Library headers.

// PCRaster library headers.

// Module headers.



namespace ag {
  // DataProperty declarations.
  class DataGuide;
}



namespace ag {



//! This class contains all visualisation properties for a piece of data.
/*!
*/
class DataProperty
{

private:

  bool             d_enabled;

  bool             d_selected;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataProperty        ();

                   DataProperty        (DataProperty const& properties);

  //                  DataProperty        (DataGuide const& guide);

  /* virtual */    ~DataProperty       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  DataProperty&    operator=           (DataProperty const& rhs);

  void             setEnabled          (bool enabled);

  void             setSelected         (bool selected);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isEnabled           () const;

  bool             isSelected          () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
