#ifndef INCLUDED_AG_DIMENSIONCOORDINATEEDIT
#define INCLUDED_AG_DIMENSIONCOORDINATEEDIT

#include <QWidget>




namespace dal {
  class Dimension;
}
namespace ag {
  // DimensionCoordinateEdit declarations.
}



namespace ag {



//! The DimensionCoordinateEdit widget provides widgets for editting the coordinate value of a dal::Dimension.
/*!
  This widget enables the user to select a new coordinate along a dimensions.
  All dimensions can be editted with this widget, it selects convenient
  widgets depending on the dimension type.

  A dimension with a dal::ExactDiscretisation discretisation can be editted with
  a combo box and a dimension with a dal::RegularDiscretisation discretisation
  with a slider.
*/
class DimensionCoordinateEdit: public QWidget
{

  friend class DimensionCoordinateEditTest;

private:

  Q_OBJECT

  //! Dimension to edit coordinate for.
  dal::Dimension const* d_dimension;

  //! Widget used to edit the coordinate.
  QWidget*         d_editWidget{nullptr};

  void             createInterface     ();

private Q_SLOTS:

  void             valueChanged        (int index);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DimensionCoordinateEdit(
                                        dal::Dimension const* dimension,
                                        QWidget* parent = nullptr);

                   DimensionCoordinateEdit(DimensionCoordinateEdit const& other) = delete;

  DimensionCoordinateEdit& operator=   (DimensionCoordinateEdit const& other) = delete;

  /* virtual */    ~DimensionCoordinateEdit() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setCoordinate       (size_t index);

  void             unsetCoordinate     ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  dal::Dimension const* dimension      () const;

Q_SIGNALS:

  /// void             coordinateChanged   (dal::Dimension const* dimension,
  ///                                       size_t index);

  void             coordinateSet       (dal::Dimension const* dimension,
                                        size_t index);

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



} // namespace ag

#endif
