#ifndef __ENVIRON
#define __ENVIRON
/***************************************************************************/
/*                                                                         */
/*  PURPOSE:  To define environment and compiler specific things           */
/***************************************************************************/

/***************************************************************************/
/*                                                                         */
/*  FILE SYSTEM STUFF                                                      */
/***************************************************************************/
/* Cygnus defines: */
#ifdef _WIN32
# ifndef DOS_FS
#  define DOS_FS
# endif
# ifndef FN_LIMIT_NONE
#  define FN_LIMIT_NONE
# endif
#endif
/* Cygnus defines: */
#ifdef i386
# ifndef CPU_LITTLE_ENDIAN
#  define CPU_LITTLE_ENDIAN
# endif
#endif

#ifdef __x86_64__
#define CPU_LITTLE_ENDIAN
#endif

/* Visual C */
#ifdef _MSC_VER
# ifndef WIN32
#  define WIN32
# endif
# ifndef DOS_FS
#  define DOS_FS
# endif
# ifndef FN_LIMIT_NONE
#  define FN_LIMIT_NONE
# endif
# ifndef CPU_LITTLE_ENDIAN
#  define CPU_LITTLE_ENDIAN
# endif
#endif

#ifdef ALPHA_OSF
 /* we define this in stddefx.h
  */
# ifndef CPU_LITTLE_ENDIAN
#  define CPU_LITTLE_ENDIAN
# endif
# ifndef UNIX_FS
#  define UNIX_FS
# endif
#endif

#ifdef DOS_FS  
# define DIR_PATH_DELIM_CHAR  '\\'
# ifdef UNIX_FS
#  error  TWO FILESYSTEMS SPECIFIED (DOS_FS, UNIX_FS)
# endif
#else
# ifdef UNIX_FS
#  define DIR_PATH_DELIM_CHAR  '/'
#  ifdef DOS_FS
#   error  TWO FILESYSTEMS SPECIFIED (DOS_FS, UNIX_FS)
#  endif
# else
#  error  NO FILESYSTEM SPECIFIED (DOS_FS, UNIX_FS)
# endif
#endif

/***************************************************************************/
/*                                                                         */
/*  CPU STUFF                                                              */
/***************************************************************************/
#ifdef INTEL32
# ifndef CPU_LITTLE_ENDIAN
#  define CPU_LITTLE_ENDIAN
# endif
#endif

#ifdef INTEL16
# ifndef CPU_LITTLE_ENDIAN
#  define CPU_LITTLE_ENDIAN
# endif
#endif

#ifdef MOTOROLA
# ifndef CPU_BIG_ENDIAN
#  define CPU_BIG_ENDIAN
# endif
#endif

#ifndef CPU_BIG_ENDIAN
# ifndef CPU_LITTLE_ENDIAN
# error NO CPU_ENDIAN SPECIFIED (CPU_BIG_ENDIAN, CPU_LITTLE_ENDIAN)
# endif
#endif

#ifdef CPU_BIG_ENDIAN
# ifdef CPU_LITTLE_ENDIAN
#  error TWO CPU_ENDIANS SPECIFIED (CPU_BIG_ENDIAN, CPU_LITTLE_ENDIAN)
# endif
#endif

#endif /* __ENVIRON */
