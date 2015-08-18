#ifndef INCLUDED_PCRLINKIN
#define INCLUDED_PCRLINKIN

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

/*
 * All rights reserved
 *  by PCRaster Environmental Software
 */

#ifdef __cplusplus
 extern "C" {
#endif

/*!
 * \brief memory buffer for pcr_LinkInExecute()
 *
 * LinkInTransferArray is used in pcr_LinkInExecute() as an array with the addresses
 * of all result and argument (input) data.
 *
 * If the callPoint to execute yield <i>r</i> results and has <i>a</i> arguments (excluding
 * an optional string argument), then the size of the array is expected to be
 * <i>r+a</i>. The first <i>r</i> positions are for the results and the next <i>a</i> positions
 * for the arguments. In other words, the first result is at position 0, and the first
 * argument at position <i>r</i>.
 *
 * All memory buffers with addresses in the array are managed (allocated and released) by
 * the PCRaster Modeling Engine. The types of the buffer are determined per buffer on
 * their respective pcrxml::result and pcrxml::argument elements
 *
 * Each buffer is of one of the following elementary types:
 *  - unsigned char*: (UINT1)  8 bit byte for pcrxml::dataType equals boolean or ldd.
 *  - int*:           (INT4)  32 bit integer for pcrxml::dataType equals nominal or ordinal.
 *  - float*:         (REAL4) 32 bit float for pcrxml::dataType equals scalar or directional.
 *  .
 *
 *  The type of the buffer is either:
 *   - the address of single value if pcrxml::spatialType equals NonSpatial
 *   - the address of an array of size pcrxml::nrRows * pcrxml::nrCols
 *     if pcrxml::spatialType equals Spatial
 *   .
 *
 * To detect and set Missing Values (e.g. NoData):
 *  - UINT1: use the value 255
 *  - INT4:  use the value -2147483647
 *  - REAL4: use a NAN value with all bits set to 1.
 *  .
 * More Missing Value information is in the PCRaster CSF file format specification.
 */
typedef void *LinkInTransferArray[];

/*!
 * \brief recieve the run context prior to each pcr_LinkInExecute() call.
 *
 * If the library defines this function then prior to each pcr_LinkInExecute() call, this
 * function is called. It enables a library programmer to store the information passed
 * by the arguments. pcr_LinkInRunContext() is intended for Simple Mode library development,
 * where the same run context information encoded in the xml argument string of pcr_LinkInExecute() can then be ignored because all information needed is passed
 * by this function.
 *
 */
#ifndef DOXYGEN_SHOULD_SKIP_THIS
 PCR_DLL_FUNC (void)
#else
 void
#endif
  pcr_LinkInRunContext(
      int nrRows,
      int nrCols,
      double cellSize,
      double xLowerLeftCorner,
      double yLowerLeftCorner,
      int timeStep);


/*!
  \brief perform typecheck for the pcrxml::callPoint found in \a xml

  This function can check a callPoint on its actual type information. Based on the
  type information feeded it can generate the possible types of the results and
  arguments. Note that this function may be called multiple times for the same 
  pcrxml::callPoint, before actual execution (pcr_LinkInExecute()).

  This function is optional in a linkin library. If not present the modelling engine
  expects all functions, classes and methods to have "simple types".

Note: pcrxml::context is currently always empty. pcr_LinkInExecute() will have pcrxml::context set.

   \param xml xml string with root-element pcrxml::linkInCheckInput
   \returns xml string with root-element pcrxml::linkInCheckResult Buffer space of pointer should be allocated by the LinkIn library and should be kept.
 */
#ifndef DOXYGEN_SHOULD_SKIP_THIS
 PCR_DLL_FUNC (const char *)
#else
 const char *
#endif
  pcr_LinkInCheck(const char *xml);

/*!
   \brief execute the pcrxml::callPoint found in \a xml

   This is the only function that MUST be defined in the library.

   \param xml  description of call, pcrxml::linkInExecuteInput root-element
   \param linkInTransferArray inputs and results
   \returns 0 if no error occured, pointer to C-string with error message if error
            occured. buffer space of pointer should be allocated by the LinkIn library
            and should be kept. If the library defines pcr_LinkInRunContext (Simple Mode) then the
            string contents is expected to be a text-string (no XML) otherwise the buffer
            should contain an XML string with pcrxml::linkInExecuteResult as root-element.
 */
#ifndef DOXYGEN_SHOULD_SKIP_THIS
 PCR_DLL_FUNC (const char *)
#else
 const char *
#endif
  pcr_LinkInExecute(
       const char *xml,
       LinkInTransferArray linkInTransferArray);


#ifdef __cplusplus
 } // extern C
#endif

#endif
