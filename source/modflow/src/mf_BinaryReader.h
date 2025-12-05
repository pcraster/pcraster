#ifndef INCLUDED_MODFLOW_BINARYREADER
#define INCLUDED_MODFLOW_BINARYREADER

#include <string>



namespace mf {
  // BinaryReader declarations.
}


namespace mf {

static const int recordMarkerSize(4);

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class BinaryReader
{

  friend class BinaryReaderTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BinaryReader               ();

                   BinaryReader               (const BinaryReader&) = delete;

  BinaryReader&    operator=                  (const BinaryReader&) = delete;

  /* virtual */    ~BinaryReader              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //void read(const std::string & err_mgs, int unit_number, float *values, const std::string description, size_t multiplier) const;
  void read(const std::string & err_mgs, const std::string & filename, float *values, const std::string& description, size_t multiplier) const;


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

} // namespace mf

#endif
