submodule (mod_mpifw) mod_broadcast_mpi

    use mpi_f08
    use iso_fortran_env

    implicit none
   
    @INTERFACE_START@
		module function broadcast_@DIM_ID@_@F_KIND_ID@(msg, np) result (res)
			@F_KIND@,intent(in) :: msg( @RANK@ )
			@F_KIND@,allocatable :: res( @RANK@ )
			integer(int32),intent(in) :: np
		end function
	@INTERFACE_END@
    
    @INTERFACE_START@
		module function broadcast_s@F_KIND_ID@(msg, np) result (res)
			@F_KIND@,intent(in) :: msg
			@F_KIND@ :: res
			integer(int32),intent(in) :: np
		end function
	@INTERFACE_END@
	
    @INTERFACE_NAME@ bcast_mpi @
    
contains
@PROCEDURE_START@
!=======================================================================
module function broadcast_@DIM_ID@_@F_KIND_ID@(msg, np) result (res)
    @F_KIND@,intent(in) :: msg( @RANK@ )
    @F_KIND@,allocatable :: res( @RANK@ )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), @MPI_KIND@, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_@DIM_ID@_@F_KIND_ID@: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
@PROCEDURE_END@
@PROCEDURE_START@
!=======================================================================
module function broadcast_s@F_KIND_ID@(msg, np) result (res)
    @F_KIND@,intent(in) :: msg
    @F_KIND@ :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, @MPI_KIND@, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_s@F_KIND_ID@: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
@PROCEDURE_END@
end submodule
