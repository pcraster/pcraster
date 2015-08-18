#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#include <windows.h>

#ifndef INCLUDED_PCRSHELL
#include "pcrshell.h"
#define INCLUDED_PCRSHELL
#endif
#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

//! open a key, or create if not existant, in PCRaster tree
/*!
   tiny wrapper around RegCreateKeyEx
   in case of error call "XXLAST WINDOWSMESSAGE"
   returns 0 if success, 1 if error.
   \todo
     Document how we handle win32 error stuff, if api does
       set last error, then we do using SetLastError
 */
extern "C" int pcr_regOpenKey(
  HKEY       *h,      // handle to topLevelKey\Software\PCRaster\keyName
  HKEY      topLevelKey, // HKEY_CURRENT_USER or HKEY_LOCAL_MACHINE
  const char *keyName)// key to open or create
{
  PRECOND(topLevelKey==HKEY_CURRENT_USER||
          topLevelKey==HKEY_LOCAL_MACHINE);
  HKEY  hPCRaster;
  DWORD dwDis;
  LONG
  result = RegCreateKeyEx(topLevelKey,"Software\\PCRaster",0,NULL,
                 REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL,
                 &hPCRaster, &dwDis);
  if (result)
    return ::Win32SetLastError(result);

  result =RegCreateKeyEx(hPCRaster,keyName,0,NULL,
                 REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL,
                 h, &dwDis);
  RegCloseKey(hPCRaster);
  if (result)
    return ::Win32SetLastError(result);
  return 0;
}

extern "C" int pcr_regOpenUserKey(
  HKEY       *h,      // handle to HKEY_CURRENT_USER\Software\PCRaster\keyName
  const char *keyName)// key to open or create
{
  return pcr_regOpenKey(h,HKEY_CURRENT_USER,keyName);
}
extern "C" int pcr_regOpenMachineKey(
  HKEY       *h,      // handle to HKEY_LOCAL_MACHINE\Software\PCRaster\keyName
  const char *keyName)// key to open or create
{
  return pcr_regOpenKey(h,HKEY_LOCAL_MACHINE,keyName);
}


//! read the start directory
/* in case of error call Win32GetLastError()
   startAsPCRDirectory is empty string if 
     - RegistryValue does not exists
     - RegistryValue is not an existing directory
   returns 0 if success, 1 if error.
 */
extern "C" int getPCRShellStartDirectory(
  char *startAsPCRDirectory) // MAX_PATH chars
{
  HKEY  hPCRShell;
  DWORD type;
  DWORD len =MAX_PATH;
  int  retVal=1;
  if (pcr_regOpenUserKey(&hPCRShell,"PCRShell"))
    return 1; /* shit happened */

  LONG result = RegQueryValueEx(hPCRShell, "RecentPCRDirectory", 0, &type,
                (BYTE *)startAsPCRDirectory,&len);
  if (result == ERROR_FILE_NOT_FOUND) {
     startAsPCRDirectory[0]='\0';
     result = 0;
  }
  if (result)
     goto error; /* and close handle, shit happened */

  if (!com::PathInfo(startAsPCRDirectory).isDirectory())
     startAsPCRDirectory[0]='\0'; // not an existing directory
  retVal=0;
error:
  RegCloseKey(hPCRShell);
  return retVal;
}

//! create the keys needed for the PCRShell
/* in case of error call Win32GetLastError()
   returns 0 if success, 1 if error.
 */
extern "C" int createPCRShellKeys(
  const char *currentPCRDirectory)
{
  HKEY  hPCRShell;
  int  retVal=1;
  if (pcr_regOpenUserKey(&hPCRShell,"PCRShell"))
    return 1; /* shit happened */

  LONG result = RegSetValueEx(hPCRShell, "RecentPCRDirectory", 0, REG_SZ,
                (const BYTE *)currentPCRDirectory,strlen(currentPCRDirectory)+1);
  if (result)
     goto error; /* and close handle, shit happened */
  retVal=0;
error:
  RegCloseKey(hPCRShell);
  return retVal;
}


extern "C" int setPCRShellDirectoryToCurrent(void)
{
  char dir[MAX_PATH];
  if (!GetCurrentDirectory(MAX_PATH,dir))
    return 1;
  return createPCRShellKeys(dir);
}
