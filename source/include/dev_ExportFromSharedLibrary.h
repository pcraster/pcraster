#ifndef INCLUDED_DEV_EXPORTFROMSHAREDLIBRARY
#define INCLUDED_DEV_EXPORTFROMSHAREDLIBRARY



// #define EXPORT_FROM_DLL __declspec(dllexport)
// #define IMPORT_FROM_DLL __declspec(dllimport)

#if defined(WIN32) || defined(_WIN32)
#  define EXPORT_FUNCTION(...) __declspec(dllexport) __VA_ARGS__ __stdcall
#  define IMPORT_FUNCTION(...) __declspec(dllimport) __VA_ARGS__ __stdcall
// #  define PCR_DLL_C             __declspec(dllexport)
#  define EXPORT_CLASS         __declspec(dllexport)
#else
#  define EXPORT_FUNCTION(...) __attribute((visibility("default"))) __VA_ARGS__
#  define IMPORT_FUNCTION      __attribute((visibility("default")))
// #  define PCR_DLL_C
#  define EXPORT_CLASS         __attribute((visibility("default")))
#endif



namespace de {

}

#endif
