module mod_allreduce_mpi

    use mpi_f08
    use iso_fortran_env
	use mod_common_mpi

    implicit none
   
    private

    interface allreduce_mpi
        procedure :: @LIST@
    end interface
    
    public :: allreduce_mpi
              
    
contains
@SCALAR_START@
!=======================================================================
function allreduce_s@F_KIND_ID@(msg, op) result (res)
    @F_KIND@,intent(in) :: msg
    character(len=*),intent(in) :: op
    @F_KIND@ :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, @MPI_KIND@, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_s@F_KIND_ID@: MPI_ALLREDUCE: ERROR")
    
end function
@SCALAR_END@
@ARRAY_START@
!=======================================================================
function allreduce_@DIM_ID@_@F_KIND_ID@(msg, op) result (res)
    @F_KIND@,intent(in) :: msg( @RANK@ )
    character(len=*),intent(in) :: op
    @F_KIND@,allocatable :: res( @RANK@ )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), @MPI_KIND@, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_@DIM_ID@_@F_KIND_ID@: MPI_ALLREDUCE: ERROR")
    
end function
@ARRAY_END@
end module
