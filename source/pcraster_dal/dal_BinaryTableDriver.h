#ifndef INCLUDED_DAL_BINARYTABLEDRIVER
#define INCLUDED_DAL_BINARYTABLEDRIVER

#include "dal_TableDriver.h"
#include "dal_TextFileDriver.h"



namespace dal {
  // BinaryTableDriver declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class PCR_DAL_DECL BinaryTableDriver: public TableDriver,
                         public TextFileDriver
{

  friend class BinaryTableDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  BinaryTableDriver& operator=         (BinaryTableDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   BinaryTableDriver   (BinaryTableDriver const& rhs);

  template<typename T>
  void             write               (Table const& table,
                                        std::ofstream& stream) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BinaryTableDriver   ();

  /* virtual */    ~BinaryTableDriver  () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  // Table*           open                (std::string const& name,
  //                                       DataSpace const& space,
  //                                       DataSpaceAddress const& address) const;

  // Table*           read                (std::string const& name,
  //                                       DataSpace const& space,
  //                                       DataSpaceAddress const& address) const;

  // void             read                (Table& table,
  //                                       std::string const& name,
  //                                       DataSpace const& space,
  //                                       DataSpaceAddress const& address) const;

  void             write               (Table const& table,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const override;

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



} // namespace dal

#endif
