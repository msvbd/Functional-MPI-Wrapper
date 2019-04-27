program test
use mod_mpi_wrapper
use iso_fortran_env
implicit none

integer(4) :: i, a(3) , b(3)    
    
call init_mpi()

    a = [( this_proc*i , i=1,3)]

    write(*,'(*(g0,1x))') this_proc,")", a

    call sync_mpi()
    if(this_proc==0) write(*,*) "=================="
    call sync_mpi()

    a = bcast_mpi(a, 3)

    write(*,'(*(g0,1x))') this_proc,")", a

call finish_mpi()
end program
