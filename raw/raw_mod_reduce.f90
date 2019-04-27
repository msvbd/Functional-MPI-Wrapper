module mod_reduce_mpi

    use mpi_f08
    use iso_fortran_env
	use mod_common_mpi

    implicit none
   
    private
    
    interface reduce_mpi
        procedure :: @LIST@
    end interface
    
    public :: reduce_mpi
              
contains
@SCALAR_START@
!=======================================================================
function reduce_s@F_KIND_ID@(what, np, op) result (res)
    @F_KIND@,intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    @F_KIND@ :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, @MPI_KIND@, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_s@F_KIND_ID@: MPI_REDUCE: ERROR")
    
end function
@SCALAR_END@
@ARRAY_START@
!======================================================================
function reduce_@DIM_ID@_@F_KIND_ID@(what, np, op) result (res)
    @F_KIND@,intent(in) :: what( @RANK@ )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    @F_KIND@,allocatable :: res( @RANK@ )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), @MPI_KIND@, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_@DIM_ID@_@F_KIND_ID@: MPI_REDUCE: ERROR")
    
end function
@ARRAY_END@
end module
