#ifndef INCLUDED_COM_INTABLESTREAM
#define INCLUDED_COM_INTABLESTREAM

#include "stddefx.h"
#include "com_tableinfo.h"
#include "com_spiritfilelineparser.h"

#include <vector>
#include <fstream>



namespace com {
  class PathName;
}



namespace com {



//! An input table stream (Deprecated use Dal)
/*!
 *  This never worked very nice, too slow. Never maded to any application.
 *  Source and tests are now no longer
 *  compiled. Also the support classes SpiritFileLineParser and TableInfo.
 *
 * inherit from TableInfo is very convenient, altough an InTableStream need to
 * HAVE a TableInfo while it IS NOT a TableInfo?
 *
 * \todo
 *   lijkt onder linux erg lang te duren, 90 MB oost dataset van Menno
 *   ca. 5 minuten tegen 1 min. van gstat. Vgl. doorlooptijd TableInfo,
 *   en pqdb::NewDataFile.read(), Vgl met lezen .bin, En parser met memory
 *   mapped ascii file
 */
class InTableStream : public TableInfo
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  InTableStream&           operator=           (const InTableStream& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   InTableStream               (const InTableStream& rhs);

  SpiritFileLineParser d_parser;
  size_t           d_nrHeaderLinesParsed;
  size_t           d_nrHeaderLinesToSkip;

  bool  moreHeaderLinesToSkip();
  void  parseHeader();

  size_t           d_lineNrStartLastRead;



public:
  typedef TableInfo::Layout        Layout;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   InTableStream               (const com::PathName& pn);

  /* virtual */    ~InTableStream              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  bool operator>>(std::vector<double>& record);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t           lineNrStartLastRead () const;
  void             throwForLastRead(const std::string& msg) const;

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



} // namespace com

#endif
