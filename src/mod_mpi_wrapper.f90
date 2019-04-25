module mod_mpi_wrapper

    use mpi_f08
    use iso_fortran_env

    implicit none
   
    private
    
    interface bcast_mpi
        procedure :: broadcast_d1r8, broadcast_d1i4, broadcast_d1r4, broadcast_d1l,&
                     broadcast_d2r8, broadcast_d2i4, broadcast_d2r4, broadcast_d2l,&
                     broadcast_d3r8, broadcast_d3i4, broadcast_d3r4, broadcast_d3l,&
                     broadcast_sr8, broadcast_si4, broadcast_sr4, broadcast_sl
    end interface
    
    interface send_mpi
        procedure :: send_d1r8, send_d1i4, send_d1r4, send_d1l,&
                     send_d2r8, send_d2i4, send_d2r4, send_d2l,&
                     send_d3r8, send_d3i4, send_d3r4, send_d3l,&
                     send_sr8, send_si4, send_sr4, send_sl
    end interface
    
    interface reduce_mpi
        procedure :: reduce_d1r8, reduce_d1i4, reduce_d1r4, reduce_d1l,&
                     reduce_d2r8, reduce_d2i4, reduce_d2r4, reduce_d2l,&
                     reduce_d3r8, reduce_d3i4, reduce_d3r4, reduce_d3l,&
                     reduce_sr8, reduce_si4, reduce_sr4, reduce_sl
    end interface
    
    interface allreduce_mpi
        procedure :: allreduce_d1r8, allreduce_d1i4, allreduce_d1r4, allreduce_d1l,&
                     allreduce_d2r8, allreduce_d2i4, allreduce_d2r4, allreduce_d2l,&
                     allreduce_d3r8, allreduce_d3i4, allreduce_d3r4, allreduce_d3l,&
                     allreduce_sr8, allreduce_si4, allreduce_sr4, allreduce_sl
    end interface
    
    type(MPI_Comm) :: comm
    type(MPI_Status) :: trans_stat
    integer :: ierr, num_procs, this_proc, idnode, tag
    
    public :: num_procs, this_proc
    protected :: num_procs, this_proc
    
    public :: init_mpi, finish_mpi, sync_mpi, stop_mpi, reduce_mpi, &
              allreduce_mpi, send_mpi, bcast_mpi
              
    
contains
!=======================================================================
function broadcast_sr8(msg, np) result (res)
    real(real64),intent(in) :: msg
    real(real64) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sr8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_sr4(msg, np) result (res)
    real(real32),intent(in) :: msg
    real(real32) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sr4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_si4(msg, np) result (res)
    integer(int32),intent(in) :: msg
    integer(int32) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_INTEGER, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_si4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_sl(msg, np) result (res)
    logical,intent(in) :: msg
    logical :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sl: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d1r8(msg, np) result (res)
    real(real64),intent(in) :: msg(:)
    real(real64),allocatable :: res(:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d1r4(msg, np) result (res)
    real(real32),intent(in) :: msg(:)
    real(real32),allocatable :: res(:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d1i4(msg, np) result (res)
    integer(int32),intent(in) :: msg(:)
    integer(int32),allocatable :: res(:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d1l(msg, np) result (res)
    logical,intent(in) :: msg(:)
    logical,allocatable :: res(:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d2r8(msg, np) result (res)
    real(real64),intent(in) :: msg(:,:)
    real(real64),allocatable :: res(:,:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d2r4(msg, np) result (res)
    real(real32),intent(in) :: msg(:,:)
    real(real32),allocatable :: res(:,:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d2i4(msg, np) result (res)
    integer(int32),intent(in) :: msg(:,:)
    integer(int32),allocatable :: res(:,:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d2l(msg, np) result (res)
    logical,intent(in) :: msg(:,:)
    logical,allocatable :: res(:,:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d3ch(msg, np) result (res)
    character(len=*),intent(in) :: msg(:,:,:)
    character(len=:),allocatable :: res(:,:,:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_CHARACTER, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3ch: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d3r8(msg, np) result (res)
    real(real64),intent(in) :: msg(:,:,:)
    real(real64),allocatable :: res(:,:,:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d3r4(msg, np) result (res)
    real(real32),intent(in) :: msg(:,:,:)
    real(real32),allocatable :: res(:,:,:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d3i4(msg, np) result (res)
    integer(int32),intent(in) :: msg(:,:,:)
    integer(int32),allocatable :: res(:,:,:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
function broadcast_d3l(msg, np) result (res)
    logical,intent(in) :: msg(:,:,:)
    logical,allocatable :: res(:,:,:)
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
!=======================================================================
!=======================================================================
function send_sl(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg
    logical :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
    
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_sl: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_sl: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_si4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg
    integer(int32) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_INTEGER,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_si4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_INTEGER,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_si4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_sr4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg
    real(real32) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_sr4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_sr4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_sr8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg
    real(real64) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_sr8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg, 1, MPI_REAL8, pfrom, tag, comm, trans_stat, ierr)
        if(ierr /= 0) call stop_mpi("send_sr8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d1l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg(:)
    logical,allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1l: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d1i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg(:)
    integer(int32),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d1r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg(:)
    real(real32),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d1r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg(:)
    real(real64),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg, size(recv_msg), MPI_REAL8, pfrom, tag, comm, trans_stat, ierr)
        if(ierr /= 0) call stop_mpi("send_d1r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d2l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg(:,:)
    logical,allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2l: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d2i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg(:,:)
    integer(int32),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d2r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg(:,:)
    real(real32),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d2r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg(:,:)
    real(real64),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg, size(recv_msg), MPI_REAL8, pfrom, tag, comm, trans_stat, ierr)
        if(ierr /= 0) call stop_mpi("send_d2r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d3l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(inout) :: send_msg(:,:,:)
    logical,allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
    
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3l: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d3i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(inout) :: send_msg(:,:,:)
    integer(int32),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d3r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(inout) :: send_msg(:,:,:)
    real(real32),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
function send_d3r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(inout) :: send_msg(:,:,:)
    real(real64),allocatable ::  recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
    
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg, size(recv_msg), MPI_REAL8, pfrom, tag, comm, trans_stat, ierr)
        if(ierr /= 0) call stop_mpi("send_d3r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
!=======================================================================
!=======================================================================
function allreduce_d3r8(what, op) result (res)
    real(real64),intent(in) :: what(:,:,:)
    character(len=*),intent(in) :: op
    real(real64) :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3r4(what, op) result (res)
    real(real32),intent(in) :: what(:,:,:)
    character(len=*),intent(in) :: op
    real(real32) :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3i4(what, op) result (res)
    integer(int32),intent(in) :: what(:,:,:)
    character(len=*),intent(in) :: op
    integer(int32) :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_INTEGER, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3l(what, op) result (res)
    logical,intent(in) :: what(:,:,:)
    character(len=*),intent(in) :: op
    logical :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3l: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2r8(what, op) result (res)
    real(real64),intent(in) :: what(:,:)
    character(len=*),intent(in) :: op
    real(real64) :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2r4(what, op) result (res)
    real(real32),intent(in) :: what(:,:)
    character(len=*),intent(in) :: op
    real(real32) :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2i4(what, op) result (res)
    integer(int32),intent(in) :: what(:,:)
    character(len=*),intent(in) :: op
    integer(int32) :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_INTEGER, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2l(what, op) result (res)
    logical,intent(in) :: what(:,:)
    character(len=*),intent(in) :: op
    logical :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2l: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1r8(what, op) result (res)
    real(real64),intent(in) :: what(:)
    character(len=*),intent(in) :: op
    real(real64) :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1r4(what, op) result (res)
    real(real32),intent(in) :: what(:)
    character(len=*),intent(in) :: op
    real(real32) :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1i4(what, op) result (res)
    integer(int32),intent(in) :: what(:)
    character(len=*),intent(in) :: op
    integer(int32) :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_INTEGER, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1l(what, op) result (res)
    logical,intent(in) :: what(:)
    character(len=*),intent(in) :: op
    logical :: res(size(what))
    
    CALL MPI_ALLREDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1l: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sr8(what, op) result (res)
    real(real64),intent(in) :: what
    character(len=*),intent(in) :: op
    real(real64) :: res
    
    CALL MPI_ALLREDUCE (what, res, 1, MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sr8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sr4(what, op) result (res)
    real(real32),intent(in) :: what
    character(len=*),intent(in) :: op
    real(real32) :: res
    
    CALL MPI_ALLREDUCE (what, res, 1, MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sr4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_si4(what, op) result (res)
    integer(int32),intent(in) :: what
    character(len=*),intent(in) :: op
    integer(int32) :: res
    
    CALL MPI_ALLREDUCE (what, res, 1, MPI_INTEGER, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_si4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sl(what, op) result (res)
    logical,intent(in) :: what
    character(len=*),intent(in) :: op
    logical :: res
    
    CALL MPI_ALLREDUCE (what, res, 1, MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sl: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
!=======================================================================
!=======================================================================
function reduce_sr8(what, np, op) result (res)
    real(real64),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_sr8: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_sr4(what, np, op) result (res)
    real(real32),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_sr4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_si4(what, np, op) result (res)
    integer(int32),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_INTEGER, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_si4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_sl(what, np, op) result (res)
    logical,intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_sl: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d1r8(what, np, op) result (res)
    real(real64),intent(in) :: what(:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64) :: res(size(what))
    
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1r8: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d1r4(what, np, op) result (res)
    real(real32),intent(in) :: what(:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32) :: res(size(what))
    
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1r4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d1i4(what, np, op) result (res)
    integer(int32),intent(in) :: what(:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32) :: res(size(what))
    
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1i4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d1l(what, np, op) result (res)
    logical,intent(in) :: what(:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical :: res(size(what))
    
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1l: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d2r8(what, np, op) result (res)
    real(real64),intent(in) :: what(:,:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res(:,:)
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2r8: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d2r4(what, np, op) result (res)
    real(real32),intent(in) :: what(:,:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res(:,:)
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2r4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d2i4(what, np, op) result (res)
    integer(int32),intent(in) :: what(:,:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res(:,:)
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2i4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d2l(what, np, op) result (res)
    logical,intent(in) :: what(:,:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical,allocatable :: res(:,:)
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2l: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d3r8(what, np, op) result (res)
    real(real64),intent(in) :: what(:,:,:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res(:,:,:)
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3r8: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d3r4(what, np, op) result (res)
    real(real32),intent(in) :: what(:,:,:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res(:,:,:)
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3r4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d3i4(what, np, op) result (res)
    integer(int32),intent(in) :: what(:,:,:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res(:,:,:)
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3i4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
function reduce_d3l(what, np, op) result (res)
    logical,intent(in) :: what(:,:,:)
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical,allocatable :: res(:,:,:)
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3l: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
!=======================================================================
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
