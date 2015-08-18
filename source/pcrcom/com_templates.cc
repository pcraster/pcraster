// Instantiate templates with the types used in the com-lib.

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

#ifndef INCLUDED_COM_COMMANDMODEARGUMENT
#include "com_commandmodeargument.h"
#define INCLUDED_COM_COMMANDMODEARGUMENT
#endif

#include "com_axis.cc"
#include "com_legendclass.cc"
#include "com_legend.cc"



template class com_Axis<REAL8>;

template class com_LegendClass<UINT1>;
template class com_LegendClass<INT4>;
template class com_LegendClass<REAL8>;

template class com::Legend<UINT1>;
template class com::Legend<INT4>;
template class com::Legend<REAL8>;


//------------------------------------------------------------------------------
// mam/puma only
// #include "com_commandlineargument.h"
// #include "com_exclusiveargument.cc"
// #include "com_repeatableargument.cc"
// #include "com_repeatableexclusiveargument.cc"
// 
// template class com::RepeatableArgument<com::PositionalValue<std::string> >;
// template class com::RepeatableArgument<com::PositionalValue<int> >;
// template class com::RepeatableArgument<com::PositionalValue<double> >;
// 
// template class com::RepeatableArgument<com::PositionalList<std::string> >;
// template class com::RepeatableArgument<com::PositionalList<int> >;
// template class com::RepeatableArgument<com::PositionalList<double> >;
// 
// template class com::RepeatableArgument<com::OptionValue<std::string> >;
// 
// template class com::RepeatableExclusiveArgument<com::OptionValue<std::string> >;
// template class com::RepeatableExclusiveArgument<com::OptionList<std::string> >;
// 
// /*
// template class com::RepeatableArgument<com::ExclusiveArgument<
//                    com::OptionList<std::string> > >;
// */
// 
// template class com::ExclusiveArgument<com::Option>;
// template class com::ExclusiveArgument<com::OptionList<std::string> >;
// template class com::ExclusiveArgument<com::OptionList<int> >;
// template class com::ExclusiveArgument<com::OptionList<double> >;
// template class com::ExclusiveArgument<com::CommandModeArgument>;
// 
//------------------------------------------------------------------------------
#include "com_raster.cc"
#include "com_singlevaluedraster.cc"

template class com::Raster<UINT1>;
template class com::Raster<INT4>;
template class com::Raster<REAL4>;
template class com::Raster<REAL8>;

template class com::SingleValuedRaster<UINT1>;
template class com::SingleValuedRaster<INT4>;
template class com::SingleValuedRaster<REAL4>;
template class com::SingleValuedRaster<REAL8>;

//------------------------------------------------------------------------------
#ifndef INCLUDED_COM_PROGRESSBAR
#include "com_progressbar.h"
#define INCLUDED_COM_PROGRESSBAR
#endif

#include "com_labeledprogresstracked.cc"
#include "com_labeledprogresstracker.cc"
#include "com_progresstracked.cc"

template class
  com::LabeledProgressTracker<com::ProgressBar>;
template class
  com::ProgressTracked<com::LabeledProgressTracker<com::ProgressBar> >;
template class
  com::LabeledProgressTracked<com::ProgressBar>;
