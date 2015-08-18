#ifndef __STDDEFX__
#define __STDDEFX__

#ifdef __cplusplus
 extern "C" {
#endif

/************************************************************************/
/*                                                                      */
/* Things that should be in every module                                */
/*   Only one time include                                              */
/*                                                                      */
/*                                                                      */
/************************************************************************/

#ifdef DJGPP
#define PLATFORM_TXT "dos32/dpmi"
# ifndef DOS_FS
#  define DOS_FS
# endif
# define INTEL32
# ifndef CPU_LITTLE_ENDIAN
#  define CPU_LITTLE_ENDIAN
# endif
# define  VA_START_ARG(x)	x
# include <unistd.h>
#else
# define  VA_START_ARG(x)	x
#endif

#ifdef __BORLANDC__
# ifndef BORLANDC
#  define BORLANDC 1
# endif
#endif

#ifdef __alpha
# define ALPHA_OSF 1
#define PLATFORM_TXT "alpha/osf"
#endif

#ifdef __MINGW32__
#define PLATFORM_TXT "win32/mingw"
#define GCC
#endif
#ifdef _MSC_VER
    #ifdef _M_X64
        #define PLATFORM_TXT "win64/msvc"
    #else
        #define PLATFORM_TXT "win32/msvc"
    #endif
#endif

#ifdef  __GNUC__
#define GCC
#endif

#ifdef  __GCC__
#define GCC
#endif

#ifdef BORLANDC
#define PLATFORM_TXT "win32"
# ifndef DOS_FS
#  define DOS_FS
# endif
# ifndef CPU_LITTLE_ENDIAN
# define CPU_LITTLE_ENDIAN
# endif
# ifndef INTEL32
#  define INTEL32
# endif
#endif


#ifdef __linux__
#ifdef __x86_64__
#define PLATFORM_TXT "linux/x86_64"
#else
#define PLATFORM_TXT "linux/32"
#endif
# ifndef INTEL32
#  define INTEL32
# endif
# ifndef UNIX_FS
#  define UNIX_FS
# endif
# ifndef CPU_LITTLE_ENDIAN
#  define CPU_LITTLE_ENDIAN
# endif
#endif

#ifdef __APPLE__
#ifdef __x86_64__
#define PLATFORM_TXT "darwin/x86_64"
#else
#define PLATFORM_TXT "darwin/32"
#endif
# ifndef INTEL32
#  define INTEL32
# endif
# ifndef UNIX_FS
#  define UNIX_FS
# endif
# ifndef CPU_LITTLE_ENDIAN
#  define CPU_LITTLE_ENDIAN
# endif
#endif

#ifdef BORLANDC
/* start exclusion of C-linkage
 * including standard headers complains in C type linking 
 */
# ifdef __cplusplus
  }
# endif
#endif

/* ANSI */
#include <stddef.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>

#ifdef BORLANDC
/* end exclusion of C-linkage
 * including standard headers complains in C type linking 
 */
# ifdef __cplusplus
 extern "C" {
# endif
#endif

/* SELF */
#include "environ.h"
#include "typedef.h"
#include "debug.h"

/***********************************************/
/* Extended library functions                  */
/***********************************************/

/* COOL for gcc?
  #define min(x, y) ({typeof (x) x_ = (x); typeof (y) y_ = (y); x_ ? y_ ? x_ : y_;}) 
 */

#ifdef MIN
# if   MIN(4,2) != 2
#  error Expected that MIN macro is the minimum of two numbers
# endif
#else
# define MIN(a,b)	(((a) < (b)) ? (a) : (b))
#endif

#ifdef MAX
# if   MAX(4,2) != 4
#  error Expected that MAX macro is the maximum of two numbers
#endif
#else
# define MAX(a,b)	(((a) > (b)) ? (a) : (b))
#endif

#define ABS(a) 		(((a) > 0) ? (a) : -(a))
#define XOR		^

/* typedef for fourth argument qsort(),bsearch(), etc.. */
typedef int (*QSORT_CMP)(const void *e1, const void *e2);

/* OBSOLOTE?: #define VOID void */
/* retype VOID to char in a module
 if calculation is required */
typedef char *VPTR;
#define ARRAY_SIZE(arrayName) (sizeof(arrayName)/sizeof(arrayName[0]))

/* STUFF to shut up compiler warnings
 */
#define USED_UNINIT_ZERO   0
#define USED_UNINIT_NULL   NULL

#ifdef __cplusplus
 /* 1) end extern "C" linkage */
 }
 /* 2) include C++ debug trace code: */
 #ifdef DEBUG_DEVELOP

  #ifndef INCLUDED_IOSTREAM
  #include <iostream>
  #define INCLUDED_IOSTREAM
  #endif

  #ifndef INCLUDED_COM_DEBUGSTREAM
  #include "com_debugstream.h"
  #define INCLUDED_COM_DEBUGSTREAM
  #endif

  #define STREAM_VAR(x) #x << ":" << x

  #define IN_BLOCK(description) \
    dbs.inBlock(description, __LINE__);
  #define OUT_BLOCK() \
    dbs.outBlock();
  #define OUT_MEMBER_FUNCTION() \
    dbs.outMemberFunction();

  #define PCRMESSAGE(msg) \
    dbs.message(msg)

  #ifdef GCC
    // more specific for gcc
    #define IN_MEMBER_FUNCTION() \
      dbs.inMemberFunction(__PRETTY_FUNCTION__);
    #define PRINT_VAR(x) \
      std::cerr << __FILE__ << ":" << __LINE__ <<":"  \
                << __PRETTY_FUNCTION__  << ": " \
                << STREAM_VAR(x) << std::endl;
  #else
    // Ansi C++ must support this
    #define IN_MEMBER_FUNCTION() \
      dbs.inMemberFunction(__FILE__,__LINE__);
    #define PRINT_VAR(x) \
      std::cerr << __FILE__ << ":" << __LINE__ <<": " \
                << STREAM_VAR(x) << std::endl;
  #endif

 #else
   #define  IN_MEMBER_FUNCTION()
   #define  OUT_MEMBER_FUNCTION()
   #define  IN_BLOCK(description)
   #define  OUT_BLOCK()
   #define  PRINT_VAR(x)
   #define  STREAM_VAR(x)
   #define  PCRMESSAGE(message)
 #endif /* DEBUG_DEVELOP */
#endif /* __cplusplus */

#ifdef NDEBUG
# define DEBUG_ARG(x)
#else
# define DEBUG_ARG(x) x
#endif

#define POSSIBLE_DATA_LOSS(smallerType, exprWithLoss) ((smallerType)(exprWithLoss))

#endif /* __STDDEFX__ */

