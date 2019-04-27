zmodule mod_broadcast_mpi

    use mpi_f08
    use iso_fortran_env
	use mod_common_mpi

    implicit none
   
    private
    
    interface bcast_mpi
        procedure :: @LIST@
    end interface

    public :: bcast_mpi
              
    
contains
@ARRAY_START@
!=======================================================================
function broadcast_@DIM_ID@_@F_KIND_ID@(msg, np) result (res)
    @F_KIND@,intent(in) :: msg( @RANK@ )
    @F_KIND@,allocatable :: res( @RANK@ )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), @MPI_KIND@, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_@DIM_ID@_@F_KIND_ID@: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
@ARRAY_END@
@SCALAR_START@
!=======================================================================
function broadcast_s@F_KIND_ID@(msg, np) result (res)
    @F_KIND@,intent(in) :: msg
    @F_KIND@ :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, @MPI_KIND@, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_s@F_KIND_ID@: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
@SCALAR_END@
end module
