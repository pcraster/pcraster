#include "numpy_conversion.h"
#include <boost/format.hpp>
#include <boost/mpl/if.hpp>
#include <boost/static_assert.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/type_traits.hpp>
#include <numpy/arrayobject.h>
#include "pcrtypes.h"
#include "geo_rasterspace.h"
#include "dal_Utils.h"
#include "calc_field.h"
#include "calc_spatial.h"
#include "value_scale_traits.h"


// From the numpy reference:
// http://docs.scipy.org/doc/numpy-1.6.0/reference/c-api.array.html#checking-the-api-version
//
//   ABI incompatibility is automatically detected in every numpy’s
//   version. API incompatibility detection was added in numpy 1.4.0. If
//   you want to supported many different numpy versions with one
//   extension binary, you have to build your extension with the lowest
//   NPY_FEATURE_VERSION as possible.
//
// This is the reason that we build the extension using numpy 1.4.
// - The // extension is compatible with a range of numpy versions and
// - version checks are handled by numpy.


namespace pcraster {
namespace python {
namespace detail {

template<typename T>
void fill_data(char * data, calc::Field const* field, bool is_spatial, size_t nr_values){
    if (is_spatial) {
        field->beMemCpySrc(data);
    }
    else {
        for (size_t i = 0; i < nr_values; ++i){
            std::memcpy(data + i * sizeof(T), field->src(), sizeof(T));
        }
    }
}

}

DEFINE_INIT_NUMPY()


//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
bpn_array field_to_array(
    geo::RasterSpace const& space,
    calc::Field const* field,
    double const missing_value)
{
    init_numpy();
    PRECOND(field->src());

    npy_intp dimensions[2];
    dimensions[0] = space.nrRows();
    dimensions[1] = space.nrCols();
    size_t nr_values = space.nrCells();

    switch(field->cr()) {
        case CR_UINT1: {
          #if BOOST_VERSION < 106500
            boost::python::object object(boost::python::handle<>(
                PyArray_SimpleNew(2, dimensions, NPY_UINT8)));
            char* data = static_cast<char*>(PyArray_DATA((PyArrayObject*)object.ptr()));
            detail::fill_data<UINT1>(data, field, field->isSpatial(), nr_values);

            dal::fromStdMV<UINT1>((UINT1*)data, nr_values,
                static_cast<UINT1>(missing_value));
            return boost::python::extract<bpn_array>(
                object);
            break;
          #else
            char *data = new char[nr_values * sizeof(UINT1)];
            detail::fill_data<UINT1>(data, field, field->isSpatial(), nr_values);

            dal::fromStdMV<UINT1>((UINT1*)data, nr_values,
                static_cast<UINT1>(missing_value));

            bpn_array mul_data_ex = boost::python::numpy::from_data(data,
                boost::python::numpy::dtype::get_builtin<uint8_t>(),
                boost::python::make_tuple(nr_values),
                boost::python::make_tuple(sizeof(uint8_t)),
                boost::python::object());

            return mul_data_ex.reshape(boost::python::make_tuple(space.nrRows(), space.nrCols()));
            break;
          #endif
        }
        case CR_INT4: {
          #if BOOST_VERSION < 106500
            boost::python::object object(boost::python::handle<>(
                PyArray_SimpleNew(2, dimensions, NPY_INT32)));
            char* data = static_cast<char*>(PyArray_DATA((PyArrayObject*)object.ptr()));
            detail::fill_data<INT4>(data, field, field->isSpatial(), nr_values);

            dal::fromStdMV<INT4>((INT4*)data, nr_values, static_cast<INT4>(
                missing_value));
            return boost::python::extract<bpn_array>(
                object);
            break;
          #else
            char *data = new char[nr_values * sizeof(INT4)];
            detail::fill_data<INT4>(data, field, field->isSpatial(), nr_values);

            dal::fromStdMV<INT4>((INT4*)data, nr_values,
                static_cast<INT4>(missing_value));

            bpn_array mul_data_ex = boost::python::numpy::from_data(data,
                boost::python::numpy::dtype::get_builtin<int32_t>(),
                boost::python::make_tuple(nr_values),
                boost::python::make_tuple(sizeof(int32_t)),
                boost::python::object());

            return mul_data_ex.reshape(boost::python::make_tuple(space.nrRows(), space.nrCols()));
            break;
          #endif
        }
        case CR_REAL4:
        default: {
          #if BOOST_VERSION < 106500
            assert(field->cr() == CR_REAL4);
            boost::python::object object(boost::python::handle<>(
                PyArray_SimpleNew(2, dimensions, NPY_FLOAT32)));
            char *data = static_cast<char*>(PyArray_DATA((PyArrayObject*)object.ptr()));
            detail::fill_data<REAL4>(data, field, field->isSpatial(), nr_values);

            dal::fromStdMV<REAL4>((REAL4*)data, nr_values, static_cast<REAL4>(
                missing_value));
            return boost::python::extract<bpn_array>(
                object);
            break;
          #else
            char *data = new char[nr_values * sizeof(REAL4)];
            detail::fill_data<REAL4>(data, field, field->isSpatial(), nr_values);

            dal::fromStdMV<REAL4>((REAL4*)data, nr_values,
                static_cast<REAL4>(missing_value));

            bpn_array mul_data_ex = boost::python::numpy::from_data(data,
                boost::python::numpy::dtype::get_builtin<float_t>(),
                boost::python::make_tuple(nr_values),
                boost::python::make_tuple(sizeof(float_t)),
                boost::python::object());

            return mul_data_ex.reshape(boost::python::make_tuple(space.nrRows(), space.nrCols()));
            break;
          #endif
        }
    }
}


#define CHECK_MINIMUM true
#define DONT_CHECK_MINIMUM false
#define CHECK_MAXIMUM true
#define DONT_CHECK_MAXIMUM false


template<
    class Source,
    class Destination,
    VS value_scale,
    bool check_minimum,
    bool check_maximum>
struct ArrayCopier
{
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct ArrayCopier<Source, Destination, value_scale,
    CHECK_MINIMUM, CHECK_MAXIMUM>
{
    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        // Source can hold destination min/max for sure.
        Source min = static_cast<Source>(
            ValueScaleTraits<value_scale>::minimum);
        Source max = static_cast<Source>(
            ValueScaleTraits<value_scale>::maximum);
        Source source_value;
        size_t const nr_values = space.nrCells();

        for(size_t i = 0; i < nr_values; ++i) {
            source_value = source[i];

            if(source_value == missing_value) {
                pcr::setMV(destination[i]);
            }
            else {
                if(source_value < min || source_value > max) {
                    typedef typename boost::mpl::if_c<
                        boost::is_same<Source, boost::uint8_t>::value ||
                        boost::is_same<Source, boost::int8_t>::value,
                    boost::int32_t, Source>::type PrintableType;
                    size_t const row = i / space.nrCols();
                    size_t const col = i - (row * space.nrCols());
                    throw std::logic_error((boost::format(
                        "Incorrect value %4% at input array [%1%][%2%] "
                        "for %3% map")
                        % row % col % ValueScaleTraits<value_scale>::name
                        % static_cast<PrintableType>(source_value)).str().c_str());
                }

                destination[i] = static_cast<Destination>(source_value);
            }
        }
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct ArrayCopier<Source, Destination, value_scale,
    DONT_CHECK_MINIMUM, DONT_CHECK_MAXIMUM>
{
    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        Source source_value;
        size_t const nr_values = space.nrCells();

        for(size_t i = 0; i < nr_values; ++i) {
            source_value = source[i];

            if(source_value == missing_value) {
                pcr::setMV(destination[i]);
            }
            else {
                destination[i] = static_cast<Destination>(source_value);
            }
        }
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct ArrayCopier<Source, Destination, value_scale,
    DONT_CHECK_MINIMUM, CHECK_MAXIMUM>
{
    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        // Source can hold destination max for sure.
        // (Source may not be able to hold destination min.)
        Source max = static_cast<Source>(
            ValueScaleTraits<value_scale>::maximum);
        Source source_value;
        size_t const nr_values = space.nrCells();

        for(size_t i = 0; i < nr_values; ++i) {
            source_value = source[i];

            if(source_value == missing_value) {
                pcr::setMV(destination[i]);
            }
            else {
                if(source_value > max) {
                    typedef typename boost::mpl::if_c<
                        boost::is_same<Source, boost::uint8_t>::value ||
                        boost::is_same<Source, boost::int8_t>::value,
                        boost::int32_t, Source>::type PrintableType;

                    size_t const row = i / space.nrCols();
                    size_t const col = i - (row * space.nrCols());
                    throw std::logic_error((boost::format(
                        "Incorrect value %4% at input array [%1%][%2%] "
                        "for %3% map")
                        % row % col % ValueScaleTraits<value_scale>::name
                        % static_cast<PrintableType>(source_value)).str().c_str());
                }

                destination[i] = static_cast<Destination>(source_value);
            }
        }
    }
};


#define SOURCE_IS_SMALLER_THAN_DESTINATION true
#define SOURCE_IS_NOT_SMALLER_THAN_DESTINATION false


template<
    class Source,
    class Destination,
    VS value_scale,
    bool source_is_smaller_than_destination>
struct SignedIntegralArrayToSignedIntegralArray
{
    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value);
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct SignedIntegralArrayToSignedIntegralArray<Source, Destination,
    value_scale, SOURCE_IS_SMALLER_THAN_DESTINATION>
{
    BOOST_STATIC_ASSERT(sizeof(Source) < sizeof(Destination));

    // int8, int16 -> int32
    BOOST_STATIC_ASSERT(
        (boost::is_same<Source, boost::int8_t>::value) ||
        (boost::is_same<Source, boost::int16_t>::value));
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::int32_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, DONT_CHECK_MINIMUM,
            DONT_CHECK_MAXIMUM>::copy(source, destination, space,
                missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct SignedIntegralArrayToSignedIntegralArray<Source, Destination,
    value_scale, SOURCE_IS_NOT_SMALLER_THAN_DESTINATION>
{
    BOOST_STATIC_ASSERT(sizeof(Source) >= sizeof(Destination));

    // int32, int64 -> int32
    BOOST_STATIC_ASSERT(
        (boost::is_same<Source, boost::int32_t>::value) ||
        (boost::is_same<Source, boost::int64_t>::value));
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::int32_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, CHECK_MINIMUM,
            CHECK_MAXIMUM>::copy(source, destination, space, missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale,
    bool source_is_smaller_than_destination>
struct UnsignedIntegralArrayToSignedIntegralArray
{
    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value);
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct UnsignedIntegralArrayToSignedIntegralArray<Source, Destination,
    value_scale, SOURCE_IS_SMALLER_THAN_DESTINATION>
{
    // uint8, uint16 -> int32
    BOOST_STATIC_ASSERT(
        (boost::is_same<Source, boost::uint8_t>::value) ||
        (boost::is_same<Source, boost::uint16_t>::value));
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::int32_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, DONT_CHECK_MINIMUM,
            DONT_CHECK_MAXIMUM>::copy(
            source, destination, space, missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct UnsignedIntegralArrayToSignedIntegralArray<Source, Destination,
    value_scale, SOURCE_IS_NOT_SMALLER_THAN_DESTINATION>
{
    // uint32, uint64 -> int32
    BOOST_STATIC_ASSERT(
        (boost::is_same<Source, boost::uint32_t>::value) ||
        (boost::is_same<Source, boost::uint64_t>::value));
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::int32_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, DONT_CHECK_MINIMUM,
            CHECK_MAXIMUM>::copy(
            source, destination, space, missing_value);
    }
};


#undef SOURCE_IS_SMALLER_THAN_DESTINATION
#undef SOURCE_IS_NOT_SMALLER_THAN_DESTINATION
#define SOURCE_IS_SIGNED true
#define SOURCE_IS_UNSIGNED false
#define DESTINATION_IS_SIGNED true
#define DESTINATION_IS_UNSIGNED false


template<
    class Source,
    class Destination,
    VS value_scale,
    bool source_is_signed,
    bool destination_is_signed>
struct IntegralArrayToIntegralArray
{
    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value);
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct IntegralArrayToIntegralArray<Source, Destination, value_scale,
    SOURCE_IS_SIGNED, DESTINATION_IS_SIGNED>
{
    // int -> int32
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::int32_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        // int -> int32
        SignedIntegralArrayToSignedIntegralArray<Source, Destination,
            value_scale, sizeof(Source) < sizeof(Destination)>::copy(
                source, destination, space, missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct IntegralArrayToIntegralArray<Source, Destination, value_scale,
    SOURCE_IS_UNSIGNED, DESTINATION_IS_UNSIGNED>
{
    // uint -> uint8
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::uint8_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, CHECK_MINIMUM,
            CHECK_MAXIMUM>::copy(source, destination, space, missing_value);
    }
};



template<
    class Source,
    class Destination,
    VS value_scale>
struct IntegralArrayToIntegralArray<Source, Destination, value_scale,
    SOURCE_IS_SIGNED, DESTINATION_IS_UNSIGNED>
{
    // int -> uint8
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::uint8_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, CHECK_MINIMUM,
            CHECK_MAXIMUM>::copy(source, destination, space, missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct IntegralArrayToIntegralArray<Source, Destination, value_scale,
    SOURCE_IS_UNSIGNED, DESTINATION_IS_SIGNED>
{
    // uint -> int32
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::int32_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        UnsignedIntegralArrayToSignedIntegralArray<Source, Destination,
            value_scale, sizeof(Source) < sizeof(Destination)>::copy(
                source, destination, space, missing_value);
    }
};


#undef SOURCE_IS_SIGNED
#undef SOURCE_IS_UNSIGNED
#undef DESTINATION_IS_SIGNED
#undef DESTINATION_IS_UNSIGNED
#define SOURCE_EQUALS_DESTINATION true
#define SOURCE_DOESNT_EQUAL_DESTINATION false


template<
    class Source,
    class Destination,
    VS value_scale,
    bool source_equals_destination>
struct FloatArrayToFloatArray
{
    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value);
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct FloatArrayToFloatArray<Source, Destination, value_scale,
    SOURCE_EQUALS_DESTINATION>
{
    // float32 -> float32
    BOOST_STATIC_ASSERT((boost::is_same<Source, float>::value));
    BOOST_STATIC_ASSERT((boost::is_same<Destination, float>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        // TODO Possible optimization: memcpy values and replace missing_value
        //      in destination.
        ArrayCopier<Source, Destination, value_scale, DONT_CHECK_MINIMUM,
            DONT_CHECK_MAXIMUM>::copy(source, destination, space,
                missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct FloatArrayToFloatArray<Source, Destination, value_scale,
    SOURCE_DOESNT_EQUAL_DESTINATION>
{
    // float64 -> float32
    BOOST_STATIC_ASSERT((boost::is_same<Source, double>::value));
    BOOST_STATIC_ASSERT((boost::is_same<Destination, float>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, DONT_CHECK_MINIMUM,
            DONT_CHECK_MAXIMUM>::copy(source, destination, space,
                missing_value);
    }
};


#undef SOURCE_EQUALS_DESTINATION
#undef SOURCE_DOESNT_EQUAL_DESTINATION
#define DESTINATION_IS_SIGNED true
#define DESTINATION_IS_UNSIGNED false

template<
    class Source,
    class Destination,
    VS value_scale,
    bool destination_is_signed>
struct FloatArrayToIntegralArray
{
    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value);
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct FloatArrayToIntegralArray<Source, Destination, value_scale,
    DESTINATION_IS_SIGNED>
{
    // float -> int32
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::int32_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, CHECK_MINIMUM,
            CHECK_MAXIMUM>::copy(source, destination, space, missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct FloatArrayToIntegralArray<Source, Destination, value_scale,
    DESTINATION_IS_UNSIGNED>
{
    // float -> uint8
    BOOST_STATIC_ASSERT((boost::is_same<Destination, boost::uint8_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, CHECK_MINIMUM,
            CHECK_MAXIMUM>::copy(source, destination, space, missing_value);
    }
};


#undef DESTINATION_IS_SIGNED
#undef DESTINATION_IS_UNSIGNED
#define SOURCE_IS_INTEGRAL true
#define SOURCE_IS_FLOAT false
#define DESTINATION_IS_INTEGRAL true
#define DESTINATION_IS_FLOAT false


template<
    class Source,
    class Destination,
    VS value_scale,
    bool source_is_integral,
    bool destination_is_integral>
struct ArrayToArray
{
    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value);
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct ArrayToArray<Source, Destination, value_scale,
    SOURCE_IS_INTEGRAL, DESTINATION_IS_INTEGRAL>
{
    // int, uint -> uint8, int32
    BOOST_STATIC_ASSERT(
        (boost::is_same<Destination, boost::uint8_t>::value) ||
        (boost::is_same<Destination, boost::int32_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        IntegralArrayToIntegralArray<Source, Destination, value_scale,
            boost::is_signed<Source>::value,
            boost::is_signed<Destination>::value>::copy(source, destination,
                space, missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct ArrayToArray<Source, Destination, value_scale,
    SOURCE_IS_FLOAT, DESTINATION_IS_FLOAT>
{
    // float -> float32
    BOOST_STATIC_ASSERT((boost::is_same<Destination, float>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        FloatArrayToFloatArray<Source, Destination, value_scale,
            boost::is_same<Source, Destination>::value>::copy(source,
                destination, space, missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct ArrayToArray<Source, Destination, value_scale,
    SOURCE_IS_INTEGRAL, DESTINATION_IS_FLOAT>
{
    // int -> float32
    BOOST_STATIC_ASSERT((boost::is_same<Destination, float>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        ArrayCopier<Source, Destination, value_scale, DONT_CHECK_MINIMUM,
            DONT_CHECK_MAXIMUM>::copy(source, destination, space,
                missing_value);
    }
};


template<
    class Source,
    class Destination,
    VS value_scale>
struct ArrayToArray<Source, Destination, value_scale,
    SOURCE_IS_FLOAT, DESTINATION_IS_INTEGRAL>
{
    // float -> uint8, int32
    BOOST_STATIC_ASSERT(
        (boost::is_same<Destination, boost::uint8_t>::value) ||
        (boost::is_same<Destination, boost::int32_t>::value));

    static void copy(
        Source const* source,
        Destination* destination,
        geo::RasterSpace const& space,
        Source const missing_value)
    {
        FloatArrayToIntegralArray<Source, Destination, value_scale,
            boost::is_signed<Destination>::value>::copy(source, destination,
                space, missing_value);
    }
};


#undef SOURCE_IS_INTEGRAL
#undef SOURCE_IS_FLOAT
#undef DESTINATION_IS_INTEGRAL
#undef DESTINATION_IS_FLOAT
#undef CHECK_MINIMUM
#undef DONT_CHECK_MINIMUM
#undef CHECK_MAXIMUM
#undef DONT_CHECK_MAXIMUM


template<
    class Source,
    VS value_scale>
calc::Spatial* array_to_field(
    geo::RasterSpace const& space,
    bpn_array const& array,
    Source const missing_value)
{
    calc::Spatial* field = new calc::Spatial(value_scale,
        ValueScaleTraits<value_scale>::cell_representation_index,
        space.nrCells());

    typedef typename ValueScaleTraits<value_scale>::Type Destination;
    Source const* source = static_cast<Source const*>(
        PyArray_DATA((PyArrayObject*)array.ptr()));
    Destination* destination = static_cast<Destination*>(field->dest());

    try {
        // Select the minimum amount of logic to convert the array value
        // correctly to field values. At compile time.
        ArrayToArray<Source, Destination, value_scale,
            boost::is_integral<Source>::value,
            boost::is_integral<Destination>::value>::copy(
                source, destination, space, missing_value);
    }
    catch(...) {
        delete field;
        throw;
    }

    return field;
}


#define ARRAY_TO_FIELD_CASE(source_type, value_scale)                          \
    case value_scale: {                                                        \
        field = array_to_field<source_type, value_scale>(                      \
            space, array, static_cast<source_type>(missing_value));            \
        break;                                                                 \
    }


#define ARRAY_TO_FIELD(source_type, value_scale)                               \
    switch(value_scale) {                                                      \
        ARRAY_TO_FIELD_CASE(source_type, VS_B)                                 \
        ARRAY_TO_FIELD_CASE(source_type, VS_L)                                 \
        ARRAY_TO_FIELD_CASE(source_type, VS_N)                                 \
        ARRAY_TO_FIELD_CASE(source_type, VS_O)                                 \
        ARRAY_TO_FIELD_CASE(source_type, VS_S)                                 \
        ARRAY_TO_FIELD_CASE(source_type, VS_D)                                 \
        default: {                                                             \
            assert(false);                                                     \
            break;                                                             \
        }                                                                      \
    }


calc::Field* array_to_field(
    geo::RasterSpace const& space,
    VS const value_scale,
    bpn_array const& array,
    double const missing_value)
{
    init_numpy();

    if(!space.valid()) {
        throw std::logic_error(
            "No valid raster defined: Set clone or load map from file");
    }
    if(PyArray_NDIM((PyArrayObject*)array.ptr()) != 2) {
        throw std::logic_error(
            "Dimension error: Rank of input array must be 2");
    }

    size_t const nr_cells_in_array =
        PyArray_DIM((PyArrayObject*)array.ptr(), 0) *
        PyArray_DIM((PyArrayObject*)array.ptr(), 1);
    size_t const nr_cells_in_field = space.nrCells();

    if(nr_cells_in_array != nr_cells_in_field){
        throw std::logic_error((boost::format(
            "Size mismatch: Number of array elements is %1%, number of raster "
            "cells is %2%")
            % nr_cells_in_array
            % nr_cells_in_field).str().c_str());
    }

    int const type = PyArray_TYPE((PyArrayObject*)array.ptr());
    calc::Spatial* field = NULL;

    // http://docs.scipy.org/doc/numpy/reference/c-api.dtype.html
    switch(type) {
        case NPY_BOOL:
        case NPY_INT8: {
            ARRAY_TO_FIELD(boost::int8_t, value_scale)
            break;
        }
        case NPY_UINT8: {
            ARRAY_TO_FIELD(boost::uint8_t, value_scale)
            break;
        }
        case NPY_INT16: {
            ARRAY_TO_FIELD(boost::int16_t, value_scale)
            break;
        }
        case NPY_UINT16: {
            ARRAY_TO_FIELD(boost::uint16_t, value_scale)
            break;
        }
        case NPY_INT32: {
            ARRAY_TO_FIELD(boost::int32_t, value_scale)
            break;
        }
        case NPY_UINT32: {
            ARRAY_TO_FIELD(boost::uint32_t, value_scale)
            break;
        }
        case NPY_INT64: {
            ARRAY_TO_FIELD(boost::int64_t, value_scale)
            break;
        }
        case NPY_UINT64: {
            ARRAY_TO_FIELD(boost::uint64_t, value_scale)
            break;
        }
        case NPY_FLOAT32: {
            ARRAY_TO_FIELD(float, value_scale)
            break;
        }
        case NPY_FLOAT64: {
            ARRAY_TO_FIELD(double, value_scale)
            break;
        }
        default: {
            throw std::logic_error((boost::format(
                "Unsupported array type")).str().c_str());
        }
    }

    return field;
}

#undef ARRAY_TO_FIELD
#undef ARRAY_TO_FIELD_CASE


//! Return a numpy array that references the original buffer of data.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
bpn_array field_as_array(
    geo::RasterSpace const& space,
    PyObject* field_object)
{
    init_numpy();
    boost::python::extract<calc::Field*> field_extractor(field_object);

    if(!field_extractor.check()) {
         throw std::logic_error("Expecting a PCRaster field");
    }

    calc::Field* field(field_extractor());
    assert(field);

    if(field->isSpatial() == false) {
        throw std::runtime_error("Argument is non-spatial, only spatial PCRaster data types are supported");
    }

    PRECOND(field->src());

#if BOOST_VERSION < 106500
    npy_intp dimensions[2];
    dimensions[0] = space.nrRows();
    dimensions[1] = space.nrCols();
    PyObject* array = 0;

    switch(field->cr()) {
      case CR_UINT1: {
        array = PyArray_SimpleNewFromData(2, dimensions, NPY_UINT8,
            const_cast<void*>(field->src()));
        break;
      }
      case CR_INT4: {
        array = PyArray_SimpleNewFromData(2, dimensions, NPY_INT32,
            const_cast<void*>(field->src()));
        break;
      }
      case CR_REAL4:
      default: {
        assert(field->cr() == CR_REAL4);
        array = PyArray_SimpleNewFromData(2, dimensions, NPY_FLOAT32,
            const_cast<void*>(field->src()));
        break;
      }
    }

    assert(array);

    auto result = PyArray_SetBaseObject((PyArrayObject*)array, field_object);
    (void)result; // Shut up compiler
    assert(result == 0);

    Py_INCREF(field_object);

    return boost::python::extract<bpn_array>(array);
#else
    switch(field->cr()) {
      case CR_UINT1: {

        bpn_array nparray = boost::python::numpy::from_data(field->dest(),
                boost::python::numpy::dtype::get_builtin<uint8_t>(),
                boost::python::make_tuple(field->nrValues()),
                boost::python::make_tuple(sizeof(uint8_t)),
                boost::python::object());
        nparray.reshape(boost::python::make_tuple(space.nrRows(), space.nrCols()));

        Py_INCREF(field_object);
        return nparray.reshape(boost::python::make_tuple(space.nrRows(), space.nrCols()));
        break;
      }
      case CR_INT4: {

        bpn_array nparray = boost::python::numpy::from_data(field->dest(),
                boost::python::numpy::dtype::get_builtin<int32_t>(),
                boost::python::make_tuple(field->nrValues()),
                boost::python::make_tuple(sizeof(int32_t)),
                boost::python::object());
        nparray.reshape(boost::python::make_tuple(space.nrRows(), space.nrCols()));

        Py_INCREF(field_object);
        return nparray.reshape(boost::python::make_tuple(space.nrRows(), space.nrCols()));
        break;
       }
       case CR_REAL4:
       default: {
        assert(field->cr() == CR_REAL4);

        bpn_array nparray = boost::python::numpy::from_data(field->dest(),
                boost::python::numpy::dtype::get_builtin<float_t>(),
                boost::python::make_tuple(field->nrValues()),
                boost::python::make_tuple(sizeof(float_t)),
                boost::python::object());
        nparray.reshape(boost::python::make_tuple(space.nrRows(), space.nrCols()));


        Py_INCREF(field_object);
        return nparray.reshape(boost::python::make_tuple(space.nrRows(), space.nrCols()));
        break;
      }
    }
#endif
}

} // namespace python
} // namespace pcraster
