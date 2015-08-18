#ifndef INCLUDED_AG_XMLVIEWITEMS
#define INCLUDED_AG_XMLVIEWITEMS



// Library headers.
#include <vector>
#include <string>

// PCRaster library headers.
#include "AguilaXSD.h"

// Module headers.



namespace ag {
  // XMLViewItems declarations.
}



namespace ag {

//! helper class to deal with the AguilaView element and its subelement
class XMLViewItems
{

  friend class XMLViewItemsTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  XMLViewItems&           operator=           (XMLViewItems const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   XMLViewItems               (XMLViewItems const& rhs);

  //! Default constructor. NOT IMPLEMENTED.
                   XMLViewItems               ();

  pcrxml::AguilaView const&      d_view;

  pcrxml::StringSet::item_sequence const& get() const;

protected:

public:
   typedef pcrxml::StringSet::item_const_iterator const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
   XMLViewItems                              (pcrxml::AguilaView const& view);


  /* virtual */    ~XMLViewItems              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const_iterator           begin          () const;
  const_iterator           end            () const;
  std::vector<std::string> items          () const;

  static void              setItems       (pcrxml::AguilaView & view,
                                           std::string const& optionName,
                                           pcrxml::StringSet const& items);
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

} // namespace ag

#endif
