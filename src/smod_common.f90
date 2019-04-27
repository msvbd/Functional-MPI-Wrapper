submodule (mod_mpifw) mod_common

    use mpi_f08
    use iso_fortran_env

    implicit none
    
    
contains
!=======================================================================
module subroutine sync_mpi()
     CALL MPI_BARRIER(comm, ierr)
    if(ierr /= 0) call stop_mpi("sync_mpi: MPI_BARRIER: ERROR")
end subroutine
!=======================================================================
module subroutine stop_mpi(msg)
    character(len=*),optional :: msg
    
    if(present(msg)) write(*,'(g0)') msg
    if(ierr /= 0) write(*,'(g0)') "stop_mpi: MPI_ABORT: ERROR"
    CALL MPI_ABORT (comm, idnode, ierr)
    
end subroutine
!=======================================================================
! Collective Operation
module function CollOp(op) result (co)
    character(len=*), intent(in) :: op
     type(MPI_Op) :: co
     
     co=MPI_OP_NULL
    
    select case(op)
        case("max")
            co=MPI_MAX
        case("min")
            co=MPI_MIN
        case("sum")
            co=MPI_SUM
        case("prod")
            co=MPI_PROD
        case("and")
            co=MPI_LAND
        case("or")
            co=MPI_LOR
        case default
            call stop_mpi("Incorect collective operation: "//op)    
    end select
    
end function
end submodule
