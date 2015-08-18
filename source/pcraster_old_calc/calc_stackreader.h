#ifndef INCLUDED_CALC_STACKREADER
#define INCLUDED_CALC_STACKREADER

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif

// Module headers.



namespace calc {
  class Spatial;
}



namespace calc {

class IoFieldStrategy;
class Compressor;


//! implements format specific strategy for reading a stack of maps
class StackReader
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  StackReader&           operator=           (const StackReader&);

  //! Copy constructor. NOT IMPLEMENTED.
                   StackReader               (const StackReader&);

  //! Io strategy used
  IoFieldStrategy * const d_strategy;

  //! name of stack
  const std::string  d_stackName;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   StackReader(
                            IoFieldStrategy *strategy,
                       const std::string& stackName);

     virtual       ~StackReader              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------


  //! return name of timestep stack item
  std::string itemName(size_t t) const;

  //! return stack name
  const std::string& stackName() const;

  void checkClone(size_t t) const;

  bool itemExists(size_t t) const;

  /*! check that item at timestep \arg t exists, has the correct format,
      equals clone and is in expected Vs set
   */
  virtual VS  checkItem(size_t t, VS expectVsSet) const=0;

  //! read values for timestep in buffer
  virtual Spatial *read(size_t atTimeStep, VS readAs, const Compressor& c) const=0;
};

template<typename MapFormat>
  class StackReaderT : public StackReader {

  //! Assignment operator. NOT IMPLEMENTED.
  StackReaderT&           operator=           (const StackReaderT&);

  //! Copy constructor. NOT IMPLEMENTED.
                   StackReaderT               (const StackReaderT&);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
    public:
                   StackReaderT(
                               IoFieldStrategy *strategy,
                               const std::string& stackName):
                     StackReader(strategy,stackName) {};

     virtual       ~StackReaderT              () {};

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  /*!
   * \todo
   *   Moet nu met specialisatie, als Esri/Band en Csf helemaal in geo zitten
   *   dan wrsl. niet meer
   */
  VS  checkItem(size_t t, VS expectVsSet) const;

  Spatial *read(size_t atTimeStep, VS readAs, const Compressor& c) const
  {
    MapFormat m(itemName(atTimeStep));
    return m.readData(readAs,c);
  }
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



} // namespace calc

#endif
