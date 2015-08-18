C     Last change:  ERB  12 Apr 2001   10:00 am
C
C  Code in this file defines values for OPEN-statement specifiers.  Some
C  of the values are extensions to ANSI Fortran 90 and 95.  One of the
C  specifiers is not included in ANSI FORTRAN 77. The included
C  specifiers are ACCESS, FORM and ACTION.
C
      CHARACTER*20 ACCESS,FORM,ACTION(2)
C
C
C  Specifiers for OPEN statements for unformatted files, which are
C  sometimes compiler specific.
C  The included specifiers are ACCESS and FORM.
C
C  ACCESS specifier --
C
C    Standard Fortran -- Use unless there is a reason to do otherwise.
      DATA ACCESS/'SEQUENTIAL'/
C
C    Non-standard Fortran that causes code compiled by Lahey or Absoft
C    Fortran on personal computers to use unstructured non-formatted
C    files. This may make it possible for the non-formatted files used
C    by MODFLOW to be used with programs that are compiled by other
C    compilers.
C      DATA ACCESS/'TRANSPARENT'/
C
C  FORM specifier --
C
C    Standard Fortran, which results in vender dependent (non-portable)
C    files.  Use unless there is a reason to do otherwise.
      DATA FORM/'UNFORMATTED'/
C
C    Non-standard Fortran that causes code compiled by Compaq (Digital)
C    Fortran on personal computers to use unstructured non-formatted
C    files.  This may make it possible for the non-formatted files used
C    by MODFLOW to be used with programs that are compiled by other
C    compilers.
C      DATA FORM/'BINARY'/
C
C
C  OPEN-statement specifiers related to file-sharing.
C
C  ACTION specifier --
C
C    Standard FORTRAN 77 -- Eliminate the ACTION= specifier from all
C    OPEN statements in the source-code files.
C
C    Standard Fortran 90 and 95 -- Use unless there is a reason to do
C    otherwise.
      DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
C
C    Non-standard Fortran that causes code compiled by the Lahey LF90
C    compiler to create files that can be shared.  For use when parallel
C    processing is used or to enable an editor to view output files
C    while the program is running.
C      DATA (ACTION(I),I=1,2)/'READ,DENYWRITE','READWRITE,DENYNONE'/
