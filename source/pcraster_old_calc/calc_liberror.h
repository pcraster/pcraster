#ifndef INCLUDED_CALC_LIBERROR
#define INCLUDED_CALC_LIBERROR

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif 

//! handle old style (C) Errors
/*! the msg is a like Error was called, catch error nested and throw a StrErrorExcep
 */
void libError(const std::string& msg) /* THROW (StrErrorExcep)*/;

//! Error (old style) already called
void throwLibError(void);

std::string getLibError(void);

extern "C" void HandleLibError(const char *msg);


#endif
