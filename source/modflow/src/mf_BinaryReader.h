#ifndef INCLUDED_MF_BINARYREADER
#define INCLUDED_MF_BINARYREADER


#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// External headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.

// Module headers.




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
class BinaryReader: private boost::noncopyable
{

  friend class BinaryReaderTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BinaryReader               ();

  /* virtual */    ~BinaryReader              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //void read(const std::string & err_mgs, int unit_number, float *values, const std::string description, size_t multiplier) const;
  void read(const std::string & err_mgs, const std::string & filename, float *values, const std::string description, size_t multiplier) const;


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
