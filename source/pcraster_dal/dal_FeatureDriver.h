#ifndef INCLUDED_DAL_FEATUREDRIVER
#define INCLUDED_DAL_FEATUREDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DRIVER
#include "dal_Driver.h"
#define INCLUDED_DAL_DRIVER
#endif

#ifndef INCLUDED_DAL_FEATURELAYER
#include "dal_FeatureLayer.h"
#define INCLUDED_DAL_FEATURELAYER
#endif



namespace dal {
  // FeatureDriver declarations.
}



namespace dal {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class PCR_DAL_DECL FeatureDriver: public Driver
{

  friend class FeatureDriverTest;

private:

  template<typename T>
  bool             extremes            (T& min,
                                        T& max,
                                        std::string const& name,
                                        DataSpace space,
                                        TypeId typeId) const;

protected:

                   FeatureDriver       (Format const& format);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~FeatureDriver      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  FeatureLayer*    open                (std::string const& name,
                                        TypeId typeId) const;

  FeatureLayer*    open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual FeatureLayer* open           (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const=0;

  DataSpace        dataSpace           (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  FeatureLayer*    read                (std::string const& name) const;

  FeatureLayer*    read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual FeatureLayer* read           (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const=0;

  void             read                (FeatureLayer& layer,
                                        std::string const& name) const;

  virtual void     read                (FeatureLayer& layer,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const=0;

  bool             extremes            (boost::any& min,
                                        boost::any& max,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
inline bool FeatureDriver::extremes(
         T& min,
         T& max,
         std::string const& name,
         DataSpace space,
         TypeId typeId) const
{
  assert(!space.hasSpace());

  bool initialised = false;
  FeatureLayer* layer = 0;

  if(space.isEmpty()) {
    layer = open(name, typeId);

    if(layer) {
      if(!layer->hasExtremes()) {
        // Extremes unknown, possibly because source has no header.
        read(*layer, name);
        layer->calculateExtremes();
      }

      if(layer->hasExtremes()) {
        min = layer->template min<T>();
        max = layer->template max<T>();
        initialised = true;
      }
    }
  }
  else {
    for(DataSpaceIterator it = space.begin(); it != space.end(); ++it) {
      layer = open(name, space, *it, typeId);

      if(layer) {
        if(!layer->hasExtremes()) {
          // Extremes unknown, possibly because source has no header.
          read(*layer, name, space, *it);
          layer->calculateExtremes();
        }

        if(layer->hasExtremes()) {
          if(!initialised) {
            min = layer->template min<T>();
            max = layer->template max<T>();
            initialised = true;
          }
          else {
            min = std::min(min, layer->template min<T>());
            max = std::max(max, layer->template max<T>());
          }
        }
      }
    }
  }

  return initialised;
}


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
