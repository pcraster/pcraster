#ifndef INCLUDED_DAL_DRIVER
#define INCLUDED_DAL_DRIVER



// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_BROWSEINFO
#include "dal_BrowseInfo.h"
#define INCLUDED_DAL_BROWSEINFO
#endif

#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_DATASPACEQUERYRESULT
#include "dal_DataSpaceQueryResult.h"
#define INCLUDED_DAL_DATASPACEQUERYRESULT
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif

#ifndef INCLUDED_DAL_FORMAT
#include "dal_Format.h"
#define INCLUDED_DAL_FORMAT
#endif

#ifndef INCLUDED_DAL_PROPERTIES
#include "dal_Properties.h"
#define INCLUDED_DAL_PROPERTIES
#endif



namespace dal {
  // Driver declarations.
  class Dataset;
  class DataSpaceAddress;
  class Table;
}



namespace dal {

//! This is the base class for i/o drivers.
/*!
  With a specialized driver a dataset can be opened, read and/or written.

  Since the same driver can be used to open, read and/or write more than one
  datasets no dataset specific state should be stored in the driver.

  Drivers have a name and a description. The name must be a unique identifier
  for the driver.

  Each driver can store i/o related properties of a dataset, for example the
  filename convention used to name files of a file bases dataset.

  Optional stuff read by a driver can be put in the properties of a dataset.
  Information about optional properties set (open(), read()) or needed
  (write()) by a driver must be documented in each relevant member function
  of each driver! Use to folowing format in the driver documentation:

  <table>
    <tr>
      <td>Name of the property</td>
      <td>Type of the property</td>
      <td>Default value, if applicable</td>
    </tr>
  </table>

  Upon writing data drivers might use default values for properties which are
  not set. Properties for which no default value can be determined must be
  set before the data can be written.

  \todo We probably want to use the name *and* the space of a dataset as the
        key of its properties. I think it is now possible to have one set of
        (conflicting) properties for soil.map and soil_1.map, ..., soil_99.map.
        This is not what we want. As a hack we might come up with a function
        to create a key given a name and a dataspace or we might want to upgrade
        the properties class. This last option enables us to create one key
        for different but similar spaces (eg subsets, dataset soil[1,10] is
        the same as soil[2,8] or even soil[20,30]).
  \todo Shouldn't the datasetProperties be an instance member instead of a
        static class member? Or shouldn't we support both? Not sure...
  \todo 
   (09:27:26) Kor de Jong:   noheader wordt opgevangen door TextTableDriver en die heeft dezelfde regel als geoeastabledriver, afgezien van de titel voorwaarde dan. heb ik net wat in commentaar bijgetikt dat ik daar niet blij van ben. de gebruiker (app of mens) moet een hint geven dat de tabel wel eens een tijdserie zou kunnen bevatten en vervolgens moeten de testen wat losser worden lijkt me. bijv unsigned integer waardes, oplopende reeks, evt onregelmatig, indien titel dan moet er time in allerlei cases / talen in voorkomen 
  (09:29:05) Cees Wesseling:   kan je ook in dal het verwachte type er in pushen, als je uit de context meer weet?  b.v. aguila --timeseries rain.tss dat is een hint v/d app expect tijd in 1e kolom, accepteer in 1e kolom oplonde integer > 0 reeks 
  (09:29:35) Cees Wesseling:   bij omissie van timestep(s) hint IN rain.tss 
  (09:31:31) Kor de Jong:   alle logica zit in de dataSpace() member van de Driver klasses. die zou een dataSpace arg kunnen krijgen met een hint erin 
  (09:31:38) Cees Wesseling:   mooi, want daar gaan we eigenlijk helemaal naar toe: een context kan meer vertellen over schijnbaar ruwe data, b.v. de XML dump van een pcrcalc model dump, of de configuratie van aguila. Ik zie het helemaal zitten.
  \todo Replace constructor with name and description by the one with the
        format argument. Let the name() en description() functions query the
        format. update Dal::driverByName to no call format().name() anymore.
*/
class PCR_DAL_DECL Driver: private boost::noncopyable
{

  friend class DriverTest;

private:

  //! Data set related properties. Key is the name/space of the dataset.
  /*!
     \todo CW does static stuff poses problems when using in linkinlibrary.
      As oliver did: use dal in his library.
   */
  static Properties d_datasetProperties;

  //! Format information of the data this driver handles.
  Format           d_format;

  //! Properties/characteristics of the driver, key is one of DAL_DRIVER_* defined in dal_propertykeys.h.
  Properties       d_properties;

protected:

                   Driver              (Format const& format);

  std::string      propertiesKey       (std::string const& name,
                                        DataSpace const& space) const;

  bool             hasProperties       (std::string const& key) const;

  Properties&      properties          ();

  Format&          format              () ;

  FilenameConvention filenameConvention(std::string const& key) const;

  FilenameConvention filenameConvention(std::string const& name,
                                        DataSpace const& space) const;

  std::string      defaultExtension    (std::string const& key) const;

  std::string      defaultExtension    (std::string const& name,
                                        DataSpace const& space) const;

public:

  static Properties& datasetProperties ();

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~Driver             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Properties const& properties         () const;

  Properties&      properties          (std::string const& key) const;

  Properties&      properties          (std::string const& name,
                                        DataSpace const& space) const;

  Format const&    format              () const;

  std::string const& name              () const;

  std::string const& description       () const;

  DatasetType      datasetType         () const;

  bool             exists              (std::string const& name) const;

  //! Returns whether a data set with \a name and \a address is present at in \a space.
  /*!
    \param     name Name of source of dataset.
    \param     space Data space of dataset.
    \param     address Address in data space.
    \return    true or false
    \exception .
    \warning   Only a quick check is performed. Even if this function returns
               true there is no garantee that this driver can actually open
               the data source.
  */
  virtual bool     exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const=0;

  Dataset*         open                (std::string const& name) const;

  DataSpaceQueryResult search          (std::string const& name,
                                        DataSpace const& space,
                                        SearchHaltCondition haltCondition) const;

  //! Creates a Dataset object for the data pointed to by \a name.
  /*!
    \param     name Name of source of dataset.
    \param     space Search space for dataset.
    \param     address Address in dataset to open.
    \return    Dataset object.
    \sa        read(std::string const&)

    This function returns 0 if the dataset could not be opened by the driver.

    Implementations only need to create specialized Dataset objects if they
    think they can read all the data in \a name. Most implementation will
    have to read some kind of header to get a feel for the dataset. There is
    no obligation to read all the data from \a name. Call read(Dataset&) on the
    driver for that purpose. When this function returns a dataset it means
    that a read(Dataset&) call might succeed.

    Some dataset properties which can be determined in an efficient way will
    be set but some might be missing. For example, the driver for reading a
    text formatted matrix or table cannot determine the number of rows in the
    data file without reading the whole file. Check the driver documentation
    for more information.
  */
  virtual Dataset* open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const=0;

  Dataset*         open                (DataSpaceQueryResult const& result) const;

  DataSpace        dataSpace           (std::string const& name) const;

  /*!
    \overload  DataSpace dataSpace(std::string const& name) const
    \param     space Data space the dataset is located in.
    \param     address Address of the data in the data space we want the data space of.
  */
  virtual DataSpace dataSpace          (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const=0;

  virtual Dataset* read                (std::string const& name) const;

  // //! Reads the data at \a address in \a name and returns a pointer to a newly created Dataset object.
  // /*!
  //   \param     name Name of source of dataset.
  //   \param     address Address to return data for.
  //   \return    Dataset object.
  //   \exception Exception If the dataset could not be read successfully.

  //   All data in at \a address in \a name is read into the Dataset object.
  // */
  // virtual Dataset* read                (std::string const& name,
  //                                       DataSpaceAddress const& address) const=0;

  virtual Dataset* read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const=0;

  void             read                (Table& table,
                                        std::string const& name,
                                        DataSpace const& space) const;

  virtual void     read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const=0;

  void             remove              (std::string const& name) const;

  //! Removes the dataset with name \a name and data space \a space.
  /*!
    \param     name Name of dataset.
    \param     space Data space of \a name.
    \return    true or false
    \todo      Implement in all classes and make pure virtual.
  */
  virtual void     remove              (std::string const& name,
                                        DataSpace space) const;

  virtual void     browse              (std::vector<BrowseInfo>& attributes,
                                        std::string const& location) const;

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
