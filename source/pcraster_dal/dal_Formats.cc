#ifndef INCLUDED_DAL_FORMATS
#include "dal_Formats.h"
#define INCLUDED_DAL_FORMATS
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Formats class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC FORMATS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FORMATS MEMBERS
//------------------------------------------------------------------------------

// //! Default constructor.
// /*!
// */
// Formats::Formats()
// {
// }
// 
// 
// 
// //! Constructor.
// /*!
//   \param     format Format to add initially.
// */
// Formats::Formats(Format const& format)
// {
//   add(format);
// }
// 
// 
// 
// //! Constructor.
// /*!
//   \param     formats Formats to add initially.
// */
// Formats::Formats(std::vector<Format> const& formats)
// {
//   add(formats);
// }
// 
// 
// 
// //! Destructor.
// /*!
// */
// Formats::~Formats()
// {
// }
// 
// 
// 
// //! Adds \a format to the collection.
// /*!
//   \param     format Format to add.
// */
// void Formats::add(Format const& format)
// {
//   d_formats.push_back(format);
// }
// 
// 
// 
// //! Adds \a formats to the collection.
// /*!
//   \param     formats Formats to add.
// */
// void Formats::add(std::vector<Format> const& formats)
// {
//   d_formats.insert(d_formats.end(), formats.begin(), formats.end());
// }
// 
// 
// 
// //! Returns the number of formats in the collection.
// /*!
//   \return    Number of formats.
// */
// bool Formats::size() const
// {
//   return d_formats.size();
// }
// 
// 
// 
// //! Returns whether the collection of formats is empty.
// /*!
//   \return    true or false
// */
// bool Formats::isEmpty() const
// {
//   return d_formats.empty();
// }



//! Returns the format with \a extension.
/*!
  \param     extension Extension of format to find.
  \return    Pointer to found format or 0 if not found.
*/
Format const* Formats::formatByExtension(
         std::string const& extension) const
{
  Format const* result = 0;

  for(size_t i = 0; i < size(); ++i) {
    if(operator[](i).extensionMatches(extension)) {
      result = &operator[](i);
      break;
    }
  }

  return result;
}



Format const* Formats::formatByName(
         std::string const& name) const
{
  Format const* result = 0;

  for(size_t i = 0; i < size(); ++i) {
    if(operator[](i).name() == name) {
      result = &operator[](i);
      break;
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

