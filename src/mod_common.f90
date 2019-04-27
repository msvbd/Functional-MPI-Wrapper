module mod_common_mpi

    use mpi_f08
    use iso_fortran_env

    implicit none
   
    public
    
    type(MPI_Comm) :: comm
    type(MPI_Status) :: trans_stat
    
    integer :: ierr, idnode, tag
    integer :: num_procs, this_proc
    
    public :: num_procs, this_proc
    protected :: num_procs, this_proc
    
    public :: sync_mpi, stop_mpi, init_mpi, finish_mpi
    
contains
!=======================================================================
subroutine init_mpi()
    comm = MPI_COMM_WORLD
    tag = 1

    call MPI_INIT(ierr)
    if(ierr /= 0) call stop_mpi("init_mpi: MPI_INIT: ERROR")
    
    call MPI_COMM_SIZE(comm, num_procs, ierr)
    if(ierr /= 0) call stop_mpi("init_mpi: MPI_COMM_SIZE: ERROR")
    
    call MPI_COMM_RANK(comm, this_proc, ierr)
    if(ierr /= 0) call stop_mpi("init_mpi: MPI_COMM_RANK: ERROR")
    
end subroutine
!=======================================================================
subroutine finish_mpi()
    call sync_mpi()
    call MPI_FINALIZE(ierr)
    if(ierr /= 0) call stop_mpi("finish_mpi: MPI_FINALIZE: ERROR")
end subroutine
!=======================================================================
subroutine sync_mpi()
     CALL MPI_BARRIER(comm, ierr)
    if(ierr /= 0) call stop_mpi("sync_mpi: MPI_BARRIER: ERROR")
end subroutine
!=======================================================================
subroutine stop_mpi(msg)
    character(len=*),optional :: msg
    
    if(present(msg)) write(*,'(g0)') msg
    if(ierr /= 0) write(*,'(g0)') "stop_mpi: MPI_ABORT: ERROR"
    CALL MPI_ABORT (comm, idnode, ierr)
    
end subroutine
!=======================================================================
! Collective Operation
function CollOp(op) result (co)
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
end module
