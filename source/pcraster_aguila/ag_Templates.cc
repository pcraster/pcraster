#include "ag_FeatureLayer.h"
#include "ag_Raster.h"
#include "ag_Table.h"
#include "ag_Vector.h"



// -----------------------------------------------------------------------------
#include "com_rcsize_t.h"
#include "com_rcptr.cc"

template class com::RCPtr<com::RCSize_t>;



// -----------------------------------------------------------------------------
#include "com_legendclass.cc"

template class com_LegendClass<UINT1>;
template class com_LegendClass<INT4>;
template class com_LegendClass<REAL8>;



// -----------------------------------------------------------------------------
#include "com_classifierimp.cc"
#include "com_linclassifier.cc"
#include "com_logclassifier.cc"
#include "com_tlogclassifier.cc"
#include "com_userdefinedclassifier.cc"
#include "com_classclassifier.cc"

template class com::ClassifierImp<REAL8>;

template class com_LinClassifier<REAL8>;
template class com_LogClassifier<REAL8>;
template class com_TLogClassifier<REAL8>;
template class com::UserDefinedClassifier<REAL8>;

template class com_ClassClassifier<UINT1>;
template class com_ClassClassifier<INT4>;



// -----------------------------------------------------------------------------
#include "ag_DataInfo.cc"
#include "ag_DataManager.cc"
#include "ag_DataObjectBase.cc"

template class ag::DataInfo<ag::Table>;
template class ag::DataInfo<ag::Raster>;
template class ag::DataInfo<ag::FeatureLayer>;
template class ag::DataInfo<ag::Vector>;

template<>
  std::vector<ag::DataObjectBase<ag::Table>::Tuple>
        ag::DataObjectBase<ag::Table>::_tuples =
        std::vector<ag::DataObjectBase<ag::Table>::Tuple>();
template<>
  std::vector<ag::DataObjectBase<ag::Raster>::Tuple>
        ag::DataObjectBase<ag::Raster>::_tuples =
        std::vector<ag::DataObjectBase<ag::Raster>::Tuple>();
template<>
  std::vector<ag::DataObjectBase<ag::FeatureLayer>::Tuple>
        ag::DataObjectBase<ag::FeatureLayer>::_tuples =
        std::vector<ag::DataObjectBase<ag::FeatureLayer>::Tuple>();

template<>
  std::vector<ag::DataObjectBase<ag::Vector>::Tuple>
        ag::DataObjectBase<ag::Vector>::_tuples =
        std::vector<ag::DataObjectBase<ag::Vector>::Tuple>();

template class ag::DataManager<ag::Raster>;

template class ag::DataObjectBase<ag::Table>;
template class ag::DataObjectBase<ag::Raster>;
template class ag::DataObjectBase<ag::FeatureLayer>;
template class ag::DataObjectBase<ag::Vector>;
#include "ag_AnimationControl.h"
#include "ag_CursorWindow.h"
#include "ag_DataPropertiesDialog.h"
#include "ag_VisualisationDialog.cc"

namespace ag {

template class VisualisationDialog<DataObject*, AnimationControl>;
template class VisualisationDialog<DataObject*, CursorWindow>;
template class VisualisationDialog<DataGuide, DataPropertiesDialog>;

}
