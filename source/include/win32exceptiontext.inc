#ifndef INCLUDED_STDIO
#include <stdio.h>     // sprintf
#define INCLUDED_STDIO
#endif

#ifndef INCLUDED_WINDOWS
#include <windows.h>
#define INCLUDED_WINDOWS
#endif

/*! \file
 *   the function in here is included twice: 1) cppunit/TestRunner.cc and
 *   2) pcrcom/win32. This to prevent that cppunit would become depended
 *   on pcrcom
 */

//! return human readable description of the Exception code
/*!
   \param e exception code returned by GetExceptionCode()
            see J.Richter Adv. Windows chapter 16

 */
static const char* win32ExceptionText(size_t e)
{
   // size_t is correct DWORD equivalent
   // PRECOND(sizeof(size_t)==4);
const char *m;
  switch(e) {
    case EXCEPTION_ACCESS_VIOLATION:
     m ="The thread attempted to read from or write to a virtual address for which it does not have the appropriate access.";break;
    case EXCEPTION_BREAKPOINT:
     m ="A breakpoint was encountered.";break;
    case EXCEPTION_DATATYPE_MISALIGNMENT:
     m ="The thread attempted to read or write data that is misaligned on hardware that does not provide alignment."; break;
    // For example, 16-bit values must be aligned on 2-byte boundaries,
    // 32-bit values on 4-byte boundaries, and so on.";
    case EXCEPTION_SINGLE_STEP:
     m ="A trace trap or other single-instruction mechanism signaled that one instruction has been executed.";break;
    case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
     m ="The thread attempted to access an array element that is out of bounds, and the underlying hardware supports bounds checking.";break;
    case EXCEPTION_FLT_DENORMAL_OPERAND:
     m ="One of the operands in a floating-point operation is denormal. A denormal value is one that is too small to represent as a standard floating-point value.";break;
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
     m ="The thread attempted to divide a floating-point value by a floating-point divisor of zero.";break;
    case EXCEPTION_FLT_INEXACT_RESULT:
     m ="The result of a floating-point operation cannot be represented exactly as a decimal fraction.";break;
    case EXCEPTION_FLT_INVALID_OPERATION:
     // This exception represents any floating-point exception not included in this list.
#ifdef DEBUG_DEVELOP
     m ="unknown floating-point exception (hint: evaluated a MV_REAL? as a float?)";break;
#else
     m ="unknown floating-point exception";break;
#endif
    case EXCEPTION_FLT_OVERFLOW:
     m ="The exponent of a floating-point operation is greater than the magnitude allowed by the corresponding type.";break;
    case EXCEPTION_FLT_STACK_CHECK:
     m ="The stack overflowed or underflowed as the result of a floating-point operation.";break;
    case EXCEPTION_FLT_UNDERFLOW:
     m ="The exponent of a floating-point operation is less than the magnitude allowed by the corresponding type.";break;
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
     m ="The thread attempted to divide an integer value by an integer divisor of zero.";break;
    case EXCEPTION_INT_OVERFLOW:
     m ="The result of an integer operation caused a carry out of the most significant bit of the result.";break;
    case EXCEPTION_PRIV_INSTRUCTION:
     m ="The thread attempted to execute an instruction whose operation is not allowed in the current machine mode.";break;
    case EXCEPTION_NONCONTINUABLE_EXCEPTION:
     m ="The thread attempted to continue execution after a noncontinuable exception occurred.";break;
    case STATUS_STACK_OVERFLOW:
     m ="Application Stack Overflow.";break;
   default:
    static char msg[32];
    sprintf(msg,"code=%x",e);
     m=msg;
   break;
  }
return m;
}
