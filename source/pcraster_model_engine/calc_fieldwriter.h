#ifndef INCLUDED_CALC_FIELDWRITER
#define INCLUDED_CALC_FIELDWRITER

#include "stddefx.h"

#include <string>



namespace calc {
  // FieldWriter declarations.
}



namespace calc {

class ASTSymbolInfo;
class SpatialPacking;
class Field;
class IOStrategy;


//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class FieldWriter
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  FieldWriter&           operator=           (const FieldWriter& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   FieldWriter               (const FieldWriter& rhs);

                   FieldWriter               ();

  const std::string      d_externalName;

protected:
  const std::string&     externalName() const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   FieldWriter               (const ASTSymbolInfo& symbol);


  virtual         ~FieldWriter              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual std::string      write(const Field* f, size_t timeStep);
  virtual void             writeOutTss(const Field* id,
                                       const Field* expr,
                                       size_t timeStep);
  virtual void             finish();
  virtual void             remove();


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};

class FileWriter : public FieldWriter
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  FileWriter&           operator=           (const FileWriter& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   FileWriter               (const FileWriter& rhs);

                   FileWriter               ();

protected:
  const IOStrategy&         d_fios;

  std::string            outputFilePath     () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   FileWriter               (const ASTSymbolInfo& symbol,
                                              const IOStrategy&       fios);


          ~FileWriter              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
};

class MemoryWriter : public FieldWriter
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  MemoryWriter&           operator=           (const MemoryWriter& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   MemoryWriter               (const MemoryWriter& rhs);

                   MemoryWriter               ();

protected:
  const IOStrategy&         d_mios;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   MemoryWriter               (const ASTSymbolInfo& symbol,
                                              const  IOStrategy&      mios);


          ~MemoryWriter              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  std::string write                           (const Field* f, size_t ) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
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

} // namespace calc

#endif
