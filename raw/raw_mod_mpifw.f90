module mod_mpifw
    use iso_fortran_env
    use mpi_f08
    implicit none
    
    private
     
    interface
@INTERFACES@
    end interface
    
@OVERLOADED_INTERFACES@
    
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
end module
