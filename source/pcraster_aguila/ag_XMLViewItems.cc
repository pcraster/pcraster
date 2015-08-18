#include "ag_XMLViewItems.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the XMLViewItems class.
*/



namespace ag {

//------------------------------------------------------------------------------

/*
class XMLViewItemsPrivate
{
public:

  XMLViewItemsPrivate()
  {
  }

  ~XMLViewItemsPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC XMLVIEWITEMS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF XMLVIEWITEMS MEMBERS
//------------------------------------------------------------------------------

XMLViewItems::XMLViewItems(
         pcrxml::AguilaView const& view)

  : d_view(view)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
XMLViewItems::XMLViewItems(
         XMLViewItems const& rhs)

  : Base(rhs)

{
}
*/



XMLViewItems::~XMLViewItems()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
XMLViewItems& XMLViewItems::operator=(
         XMLViewItems const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



pcrxml::StringSet::item_sequence const& XMLViewItems::get() const
{
  if (d_view.map().present())
     return d_view.map().get().item();
  if (d_view.drape().present())
     return d_view.drape().get().item();
  if (d_view.timeGraph().present())
     return d_view.timeGraph().get().item();
  if (d_view.probabilityGraph().present())
     return d_view.probabilityGraph().get().item();
  if (d_view.valueOnly().present())
     return d_view.valueOnly().get().item();
  if (d_view.default_().present())
     return d_view.default_().get().item();
  assert(false);
  return d_view.default_().get().item();
}



XMLViewItems::const_iterator XMLViewItems::begin() const {
      return get().begin();
}


XMLViewItems::const_iterator XMLViewItems::end() const {
      return get().end();
}



//! items as vector of strings
std::vector<std::string> XMLViewItems::items() const {
     return std::vector<std::string>(begin(),end());
}



//! set \a items of \a view from command line \a optionName
void XMLViewItems::setItems(
     pcrxml::AguilaView & view,
     std::string const& optionName,
     pcrxml::StringSet const& items)
{
  if(optionName == "mapView") {
    view.map(items);
  }
  else if(optionName == "drapeView") {
    view.drape(items);
  }
  else if(optionName == "timeGraphView") {
    view.timeGraph(items);
  }
  else if(optionName == "probabilityGraphView") {
    view.probabilityGraph(items);
  }
  else if(optionName == "valueOnly") {
    view.valueOnly(items);
  }
  else if(optionName == "defaultView") {
    view.default_(items);
  }
  else {
    assert(false);
  }
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

