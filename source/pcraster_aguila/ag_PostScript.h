#ifndef INCLUDED_AG_POSTSCRIPT
#define INCLUDED_AG_POSTSCRIPT



#include <iostream>
#include <boost/filesystem/path.hpp>

#ifndef INCLUDED_AG_FEEDBACK
#include "ag_Feedback.h"
#endif



namespace ag {
  class PostScriptPrivate;
}



namespace ag {



//! The PostScript class is for the conversion to PostScript format.
/*!
  Using this class you can create and save Encapsulated PostScript files.

  \todo Sorting of the primitives!
  \todo Add primitives (point and lines)
*/
class PostScript
{

private:

  PostScriptPrivate* d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  PostScript&      operator=           (const PostScript&);

  //! Copy constructor. NOT IMPLEMENTED.
                   PostScript          (const PostScript&);

  size_t           nrPrimitives        () const;

  void             deletePrimitivesArray();

  void             createPrimitivesArray();

  void             fillPrimitivesArray ();

  void             sortPrimitivesArray ();

  void             writeHeader         (std::ostream& os);

  void             writeBody           (std::ostream& os);

  void             writeFooter         (std::ostream& os);

  Feedback::const_iterator writePrimitive(std::ostream& os,
                                        Feedback::const_iterator it);
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PostScript          (const Feedback& f,
                                        double llx,
                                        double lly,
                                        double urx,
                                        double ury);


  /* virtual */    ~PostScript         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             sort                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             save                (boost::filesystem::path const& path);

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
