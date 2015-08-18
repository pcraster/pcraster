#ifndef INCLUDED_DAL_FEATUREPATH
#define INCLUDED_DAL_FEATUREPATH



// External headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifdef DEBUG
  #ifndef INCLUDED_IOSTREAM
  #include <iostream>
  #define INCLUDED_IOSTREAM
  #endif
#endif

// Project headers.

// Module headers.



namespace dal {
  // FeaturePath declarations.
}



namespace dal {

//! This class encapsulates feature path information.
/*!
  A feature path is a path to a feature layer in a feature data set. This path
  consists of a path to a data source, a layer name and, optionally, an
  attribute name.

  \sa        .
*/
class FeaturePath
{

  friend class FeaturePathTest;
  friend bool operator==(FeaturePath const&, FeaturePath const&);
  friend bool operator!=(FeaturePath const&, FeaturePath const&);
#ifdef DEBUG
  friend std::ostream& operator<<(std::ostream&, FeaturePath const&);
#endif

private:

  //! Whether the path's contents are valid.
  bool             _isValid;

  //! Name of the data source.
  std::string      _source;

  //! Name of the layer within the data source.
  std::string      _layer;

  //! Name of the attribute within the feature layer.
  std::string      _attribute;

  bool             compare             (FeaturePath const& rhs) const;

protected:

public:

  enum ParseStrategy {
    //! Assume an attribute is present in the path.
    WithAttribute,

    //! Assume no attribute is present in the path.
    WithoutAttribute
  };

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FeaturePath         ();

                   FeaturePath         (FeaturePath const& rhs);

                   FeaturePath         (std::string const& path,
                                        ParseStrategy strategy);

  /* virtual */    ~FeaturePath        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  FeaturePath&     operator=           (FeaturePath const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isValid             () const;

  std::string const& source            () const;

  std::string const& layer             () const;

  std::string const& attribute         () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (FeaturePath const& lhs,
                                        FeaturePath const& rhs);

bool               operator!=          (FeaturePath const& lhs,
                                        FeaturePath const& rhs);

#ifdef DEBUG
std::ostream&      operator<<          (std::ostream& stream,
                                        FeaturePath const& path);
#endif



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
