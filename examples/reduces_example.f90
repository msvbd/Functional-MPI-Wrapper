program test
use iso_fortran_env
use mod_mpifw
implicit none

integer(4) :: i, a(3) , b(3)    
    
call init_mpi()

    a = [( this_proc*i , i=1,3)]

    write(*,'(*(g0,1x))') this_proc,")", a

    call sync_mpi()
    if(this_proc==0) write(*,*) "=================="
    call sync_mpi()

    write(*,'(*(g0,1x))') this_proc,")",reduce_mpi(a, 0, "sum")

    call sync_mpi()
    if(this_proc==0) write(*,*) "=================="
    call sync_mpi()

    write(*,'(*(g0,1x))') this_proc,")",allreduce_mpi(a, "sum")

call finish_mpi()
end program
