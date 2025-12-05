#ifndef INCLUDED_OLDCALC_LIBERROR
#define INCLUDED_OLDCALC_LIBERROR

#include <string>


//! handle old style (C) Errors
/*! the msg is a like Error was called, catch error nested and throw a StrErrorExcep
 */
void libError(const std::string& msg) /* THROW (StrErrorExcep)*/;

//! Error (old style) already called
void throwLibError();

std::string getLibError();

extern "C" void HandleLibError(const char *msg);


#endif
