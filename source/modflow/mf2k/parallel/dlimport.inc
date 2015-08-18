C     Last change:  ERB  18 Apr 2001   12:20 pm
C     The following DLL_IMPORT command may be required for using
C     NT-MPICH with MODFLOW-2000 when compiling with Lahey LF90 or LF95.
C     NT-MPICH can be used under the NT operating system to support the
C     MPI calls required for parallel processing with MODFLOW-2000.
C     NT-MPICH may be obtained free of charge from the URL:
C     http://www.lfbs.rwth-aachen.de/~joachim/MP-MPICH.html
C
C     The DLL_IMPORT command is an extension to Fortran provided by
C     Lahey. Other compilers may provide alternative ways of importing a
C     DLL.
C
C     For compiling under the Unix operating system, the following
C     command may be commented out or the "INCLUDE 'dlimport.inc'"
C     statements in para-mpi.f may be commented out.
C
cc      DLL_IMPORT MPI_Init, MPI_Comm_rank, MPI_Comm_size, MPI_Bcast,
cc     &           MPI_Barrier, MPI_Send, MPI_Recv, MPI_Finalize,
cc     &           MPI_Allreduce, MPI_Abort

