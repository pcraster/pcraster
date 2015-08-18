#include "ag_VisualisationDialog.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the VisualisationDialog class.
*/



namespace ag {

//------------------------------------------------------------------------------

/*
class VisualisationDialogPrivate
{
public:

  VisualisationDialogPrivate()
  {
  }

  ~VisualisationDialogPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC VISUALISATIONDIALOG MEMBERS
//------------------------------------------------------------------------------

template<class T, class Dialog>
std::map<DataObject*, std::map<T, Dialog*> >
         VisualisationDialog<T, Dialog>::d_dialogs;



template<class T, class Dialog>
bool VisualisationDialog<T, Dialog>::instanceCreated(
         DataObject* dataObject,
         T object)
{
  return d_dialogs.find(dataObject) != d_dialogs.end() &&
         d_dialogs[dataObject].find(object) != d_dialogs[dataObject].end();
}



template<class T, class Dialog>
Dialog* VisualisationDialog<T, Dialog>::instance(
         DataObject* dataObject,
         T object)
{
  return instanceCreated(dataObject, object)
         ? d_dialogs[dataObject][object]
         : 0;
}



template<class T, class Dialog>
void VisualisationDialog<T, Dialog>::addInstance(
         DataObject* dataObject,
         T object,
         Dialog* dialog)
{
  assert(!instanceCreated(dataObject, object));
  d_dialogs[dataObject][object] = dialog;
  assert(instanceCreated(dataObject, object));
}



template<class T, class Dialog>
void VisualisationDialog<T, Dialog>::removeReference(
         VisualisationDialog<T, Dialog>* dialog)
{
  typedef std::map<T, Dialog*> ObjectMap;
  typedef std::map<DataObject*, ObjectMap> DataObjectMap;

  for(typename DataObjectMap::iterator it1 = d_dialogs.begin();
         it1 != d_dialogs.end(); ++it1) {
    for(typename ObjectMap::iterator it2 = (*it1).second.begin();
         it2 != (*it1).second.end();) {
      if((*it2).second == dialog) {
        (*it1).second.erase(it2++); // Erase invalidates the iterator.
      }
      else {
        ++it2;
      }
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF VISUALISATIONDIALOG MEMBERS
//------------------------------------------------------------------------------

template<class T, class Dialog>
VisualisationDialog<T, Dialog>::VisualisationDialog (
         DataObject* object,
         std::string const& visualisationName,
         QWidget* parent,
         bool modal,
         Qt::WindowFlags flags)

  : QDialog(parent, flags),
    IVisualisation(object, visualisationName)

{
  setModal(modal);
  setWindowTitle(this->visualisationName().c_str());
  setWindowIconText(this->visualisationName().c_str());
}



/* NOT IMPLEMENTED
//! Copy constructor.
VisualisationDialog::VisualisationDialog(
         VisualisationDialog const& rhs)

  : Base(rhs)

{
}
*/



template<class T, class Dialog>
VisualisationDialog<T, Dialog>::~VisualisationDialog()
{
  removeReference(this);
}



/* NOT IMPLEMENTED
//! Assignment operator.
VisualisationDialog& VisualisationDialog::operator=(
         VisualisationDialog const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



template<class T, class Dialog>
bool VisualisationDialog<T, Dialog>::close()
{
  return QDialog::close();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

