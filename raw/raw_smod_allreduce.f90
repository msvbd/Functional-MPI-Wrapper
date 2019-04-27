submodule (mod_mpifw) mod_allreduce_mpi

    use mpi_f08
    use iso_fortran_env

    implicit none
   
    @INTERFACE_START@
    module function allreduce_s@F_KIND_ID@(msg, op) result (res)
		@F_KIND@,intent(in) :: msg
		character(len=*),intent(in) :: op
		@F_KIND@ :: res
    end function
	@INTERFACE_END@
    
    @INTERFACE_START@
		module function allreduce_@DIM_ID@_@F_KIND_ID@(msg, op) result (res)
			@F_KIND@,intent(in) :: msg( @RANK@ )
			character(len=*),intent(in) :: op
			@F_KIND@,allocatable :: res( @RANK@ )
		end function
	@INTERFACE_END@
	
    @INTERFACE_NAME@ allreduce_mpi @             
    
contains
@PROCEDURE_START@
!=======================================================================
module function allreduce_s@F_KIND_ID@(msg, op) result (res)
    @F_KIND@,intent(in) :: msg
    character(len=*),intent(in) :: op
    @F_KIND@ :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, @MPI_KIND@, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_s@F_KIND_ID@: MPI_ALLREDUCE: ERROR")
    
end function
@PROCEDURE_END@
@PROCEDURE_START@
!=======================================================================
module function allreduce_@DIM_ID@_@F_KIND_ID@(msg, op) result (res)
    @F_KIND@,intent(in) :: msg( @RANK@ )
    character(len=*),intent(in) :: op
    @F_KIND@,allocatable :: res( @RANK@ )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), @MPI_KIND@, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_@DIM_ID@_@F_KIND_ID@: MPI_ALLREDUCE: ERROR")
    
end function
@PROCEDURE_END@
end submodule
