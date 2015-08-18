#ifndef INCLUDED_COM_FILEFORMATINFO
#define INCLUDED_COM_FILEFORMATINFO



// Library headers.
#include <string>
#include <vector>

// PCRaster library headers.

// Module headers.



namespace com {
  // FileFormatInfo declarations.
}



namespace com {



//! The FileFormatInfo class if for information about file formats(!).
/*!
  Each file format has specific information like default extension, description
  and so on. This stuff is handled by this class.

  \todo
    add default basename for saving method/ctor

  \todo Merge this class with dal::Format and remove usage of this class from
        all code. dal::Format is more general.
*/
class FileFormatInfo
{

private:

  //! Description.
  std::string      d_description;

  //! Default extensions.
  std::vector<std::string> d_extensions;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FileFormatInfo      (const std::string& description,
                                        const std::string& extensions);

                   FileFormatInfo      (std::string const& description,
                                        std::vector<std::string> const& extensions);

  virtual         ~FileFormatInfo     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             equals              (const FileFormatInfo& formatInfo) const;

  const std::string& description       () const;

  const std::string& extension         () const;

  const std::vector<std::string>& extensions() const;

  // bool                 matchesExtension(const std::string& extension) const;

  static FileFormatInfo eps();
  static FileFormatInfo png();
  static FileFormatInfo csf();
  static FileFormatInfo pcr();

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (const FileFormatInfo& lhs,
                                        const FileFormatInfo& rhs);

bool               operator!=          (const FileFormatInfo& lhs,
                                        const FileFormatInfo& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
