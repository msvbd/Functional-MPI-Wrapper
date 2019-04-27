submodule (mod_mpifw) mod_reduce_mpi

    use mpi_f08
    use iso_fortran_env

    implicit none
    
    @INTERFACE_START@
		module function reduce_s@F_KIND_ID@(what, np, op) result (res)
			@F_KIND@,intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			@F_KIND@ :: res
		end function
	@INTERFACE_END@
    
    @INTERFACE_START@
		module function reduce_@DIM_ID@_@F_KIND_ID@(what, np, op) result (res)
			@F_KIND@,intent(in) :: what( @RANK@ )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			@F_KIND@,allocatable :: res( @RANK@ )
		end function
	@INTERFACE_END@
	
    @INTERFACE_NAME@ reduce_mpi @
              
contains
@PROCEDURE_START@
!=======================================================================
module function reduce_s@F_KIND_ID@(what, np, op) result (res)
    @F_KIND@,intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    @F_KIND@ :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, @MPI_KIND@, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_s@F_KIND_ID@: MPI_REDUCE: ERROR")
    
end function
@PROCEDURE_END@
@PROCEDURE_START@
!======================================================================
module function reduce_@DIM_ID@_@F_KIND_ID@(what, np, op) result (res)
    @F_KIND@,intent(in) :: what( @RANK@ )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    @F_KIND@,allocatable :: res( @RANK@ )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), @MPI_KIND@, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_@DIM_ID@_@F_KIND_ID@: MPI_REDUCE: ERROR")
    
end function
@PROCEDURE_END@
end submodule
