submodule (mod_mpifw) mod_send_mpi

    use mpi_f08
    use iso_fortran_env

    implicit none
   


    
contains
!=======================================================================
module function send_si1(send_msg, pfrom, pto) result (recv_msg)
    integer(int8),intent(in) :: send_msg
    integer(int8) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_INTEGER1,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_si1: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_INTEGER1,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_si1: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_si2(send_msg, pfrom, pto) result (recv_msg)
    integer(int16),intent(in) :: send_msg
    integer(int16) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_INTEGER2,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_si2: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_INTEGER2,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_si2: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_si4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg
    integer(int32) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_INTEGER4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_si4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_INTEGER4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_si4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_si8(send_msg, pfrom, pto) result (recv_msg)
    integer(int64),intent(in) :: send_msg
    integer(int64) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_INTEGER8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_si8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_INTEGER8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_si8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_sr4(send_msg, pfrom, pto) result (recv_msg)
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
module function send_sr8(send_msg, pfrom, pto) result (recv_msg)
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
        call MPI_RECV(recv_msg,1,MPI_REAL8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_sr8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_sr16(send_msg, pfrom, pto) result (recv_msg)
    real(real128),intent(in) :: send_msg
    real(real128) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_REAL16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_sr16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_REAL16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_sr16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_sc4(send_msg, pfrom, pto) result (recv_msg)
    complex(real32),intent(in) :: send_msg
    complex(real32) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_COMPLEX8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_sc4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_COMPLEX8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_sc4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_sc8(send_msg, pfrom, pto) result (recv_msg)
    complex(real64),intent(in) :: send_msg
    complex(real64) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_COMPLEX16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_sc8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_COMPLEX16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_sc8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_sc16(send_msg, pfrom, pto) result (recv_msg)
    complex(real128),intent(in) :: send_msg
    complex(real128) :: recv_msg
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    recv_msg = send_msg
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,1,MPI_COMPLEX32,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_sc16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,1,MPI_COMPLEX32,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_sc16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_sl(send_msg, pfrom, pto) result (recv_msg)
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
module function send_d1_i1(send_msg, pfrom, pto) result (recv_msg)
    integer(int8),intent(in) :: send_msg(:)
    integer(int8),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER1,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_i1: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER1,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_i1: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_i1(send_msg, pfrom, pto) result (recv_msg)
    integer(int8),intent(in) :: send_msg(:,:)
    integer(int8),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER1,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_i1: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER1,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_i1: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_i1(send_msg, pfrom, pto) result (recv_msg)
    integer(int8),intent(in) :: send_msg(:,:,:)
    integer(int8),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER1,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_i1: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER1,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_i1: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_i1(send_msg, pfrom, pto) result (recv_msg)
    integer(int8),intent(in) :: send_msg(:,:,:,:)
    integer(int8),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER1,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_i1: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER1,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_i1: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_i1(send_msg, pfrom, pto) result (recv_msg)
    integer(int8),intent(in) :: send_msg(:,:,:,:,:)
    integer(int8),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER1,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_i1: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER1,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_i1: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_i1(send_msg, pfrom, pto) result (recv_msg)
    integer(int8),intent(in) :: send_msg(:,:,:,:,:,:)
    integer(int8),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER1,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_i1: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER1,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_i1: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_i1(send_msg, pfrom, pto) result (recv_msg)
    integer(int8),intent(in) :: send_msg(:,:,:,:,:,:,:)
    integer(int8),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER1,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_i1: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER1,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_i1: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_i2(send_msg, pfrom, pto) result (recv_msg)
    integer(int16),intent(in) :: send_msg(:)
    integer(int16),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER2,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_i2: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER2,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_i2: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_i2(send_msg, pfrom, pto) result (recv_msg)
    integer(int16),intent(in) :: send_msg(:,:)
    integer(int16),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER2,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_i2: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER2,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_i2: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_i2(send_msg, pfrom, pto) result (recv_msg)
    integer(int16),intent(in) :: send_msg(:,:,:)
    integer(int16),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER2,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_i2: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER2,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_i2: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_i2(send_msg, pfrom, pto) result (recv_msg)
    integer(int16),intent(in) :: send_msg(:,:,:,:)
    integer(int16),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER2,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_i2: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER2,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_i2: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_i2(send_msg, pfrom, pto) result (recv_msg)
    integer(int16),intent(in) :: send_msg(:,:,:,:,:)
    integer(int16),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER2,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_i2: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER2,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_i2: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_i2(send_msg, pfrom, pto) result (recv_msg)
    integer(int16),intent(in) :: send_msg(:,:,:,:,:,:)
    integer(int16),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER2,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_i2: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER2,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_i2: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_i2(send_msg, pfrom, pto) result (recv_msg)
    integer(int16),intent(in) :: send_msg(:,:,:,:,:,:,:)
    integer(int16),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER2,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_i2: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER2,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_i2: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg(:)
    integer(int32),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg(:,:)
    integer(int32),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg(:,:,:)
    integer(int32),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg(:,:,:,:)
    integer(int32),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg(:,:,:,:,:)
    integer(int32),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg(:,:,:,:,:,:)
    integer(int32),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_i4(send_msg, pfrom, pto) result (recv_msg)
    integer(int32),intent(in) :: send_msg(:,:,:,:,:,:,:)
    integer(int32),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_i4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_i4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_i8(send_msg, pfrom, pto) result (recv_msg)
    integer(int64),intent(in) :: send_msg(:)
    integer(int64),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_i8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_i8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_i8(send_msg, pfrom, pto) result (recv_msg)
    integer(int64),intent(in) :: send_msg(:,:)
    integer(int64),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_i8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_i8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_i8(send_msg, pfrom, pto) result (recv_msg)
    integer(int64),intent(in) :: send_msg(:,:,:)
    integer(int64),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_i8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_i8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_i8(send_msg, pfrom, pto) result (recv_msg)
    integer(int64),intent(in) :: send_msg(:,:,:,:)
    integer(int64),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_i8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_i8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_i8(send_msg, pfrom, pto) result (recv_msg)
    integer(int64),intent(in) :: send_msg(:,:,:,:,:)
    integer(int64),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_i8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_i8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_i8(send_msg, pfrom, pto) result (recv_msg)
    integer(int64),intent(in) :: send_msg(:,:,:,:,:,:)
    integer(int64),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_i8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_i8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_i8(send_msg, pfrom, pto) result (recv_msg)
    integer(int64),intent(in) :: send_msg(:,:,:,:,:,:,:)
    integer(int64),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_INTEGER8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_i8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_INTEGER8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_i8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg(:)
    real(real32),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg(:,:)
    real(real32),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg(:,:,:)
    real(real32),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg(:,:,:,:)
    real(real32),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg(:,:,:,:,:)
    real(real32),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg(:,:,:,:,:,:)
    real(real32),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_r4(send_msg, pfrom, pto) result (recv_msg)
    real(real32),intent(in) :: send_msg(:,:,:,:,:,:,:)
    real(real32),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL4,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_r4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL4,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_r4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg(:)
    real(real64),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg(:,:)
    real(real64),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg(:,:,:)
    real(real64),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg(:,:,:,:)
    real(real64),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg(:,:,:,:,:)
    real(real64),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg(:,:,:,:,:,:)
    real(real64),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_r8(send_msg, pfrom, pto) result (recv_msg)
    real(real64),intent(in) :: send_msg(:,:,:,:,:,:,:)
    real(real64),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_r8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_r8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_r16(send_msg, pfrom, pto) result (recv_msg)
    real(real128),intent(in) :: send_msg(:)
    real(real128),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_r16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_r16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_r16(send_msg, pfrom, pto) result (recv_msg)
    real(real128),intent(in) :: send_msg(:,:)
    real(real128),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_r16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_r16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_r16(send_msg, pfrom, pto) result (recv_msg)
    real(real128),intent(in) :: send_msg(:,:,:)
    real(real128),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_r16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_r16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_r16(send_msg, pfrom, pto) result (recv_msg)
    real(real128),intent(in) :: send_msg(:,:,:,:)
    real(real128),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_r16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_r16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_r16(send_msg, pfrom, pto) result (recv_msg)
    real(real128),intent(in) :: send_msg(:,:,:,:,:)
    real(real128),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_r16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_r16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_r16(send_msg, pfrom, pto) result (recv_msg)
    real(real128),intent(in) :: send_msg(:,:,:,:,:,:)
    real(real128),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_r16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_r16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_r16(send_msg, pfrom, pto) result (recv_msg)
    real(real128),intent(in) :: send_msg(:,:,:,:,:,:,:)
    real(real128),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_REAL16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_r16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_REAL16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_r16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_c4(send_msg, pfrom, pto) result (recv_msg)
    complex(real32),intent(in) :: send_msg(:)
    complex(real32),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_c4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_c4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_c4(send_msg, pfrom, pto) result (recv_msg)
    complex(real32),intent(in) :: send_msg(:,:)
    complex(real32),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_c4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_c4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_c4(send_msg, pfrom, pto) result (recv_msg)
    complex(real32),intent(in) :: send_msg(:,:,:)
    complex(real32),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_c4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_c4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_c4(send_msg, pfrom, pto) result (recv_msg)
    complex(real32),intent(in) :: send_msg(:,:,:,:)
    complex(real32),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_c4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_c4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_c4(send_msg, pfrom, pto) result (recv_msg)
    complex(real32),intent(in) :: send_msg(:,:,:,:,:)
    complex(real32),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_c4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_c4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_c4(send_msg, pfrom, pto) result (recv_msg)
    complex(real32),intent(in) :: send_msg(:,:,:,:,:,:)
    complex(real32),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_c4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_c4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_c4(send_msg, pfrom, pto) result (recv_msg)
    complex(real32),intent(in) :: send_msg(:,:,:,:,:,:,:)
    complex(real32),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX8,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_c4: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX8,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_c4: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_c8(send_msg, pfrom, pto) result (recv_msg)
    complex(real64),intent(in) :: send_msg(:)
    complex(real64),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_c8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_c8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_c8(send_msg, pfrom, pto) result (recv_msg)
    complex(real64),intent(in) :: send_msg(:,:)
    complex(real64),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_c8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_c8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_c8(send_msg, pfrom, pto) result (recv_msg)
    complex(real64),intent(in) :: send_msg(:,:,:)
    complex(real64),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_c8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_c8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_c8(send_msg, pfrom, pto) result (recv_msg)
    complex(real64),intent(in) :: send_msg(:,:,:,:)
    complex(real64),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_c8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_c8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_c8(send_msg, pfrom, pto) result (recv_msg)
    complex(real64),intent(in) :: send_msg(:,:,:,:,:)
    complex(real64),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_c8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_c8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_c8(send_msg, pfrom, pto) result (recv_msg)
    complex(real64),intent(in) :: send_msg(:,:,:,:,:,:)
    complex(real64),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_c8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_c8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_c8(send_msg, pfrom, pto) result (recv_msg)
    complex(real64),intent(in) :: send_msg(:,:,:,:,:,:,:)
    complex(real64),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX16,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_c8: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX16,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_c8: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_c16(send_msg, pfrom, pto) result (recv_msg)
    complex(real128),intent(in) :: send_msg(:)
    complex(real128),allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX32,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_c16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX32,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_c16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_c16(send_msg, pfrom, pto) result (recv_msg)
    complex(real128),intent(in) :: send_msg(:,:)
    complex(real128),allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX32,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_c16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX32,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_c16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_c16(send_msg, pfrom, pto) result (recv_msg)
    complex(real128),intent(in) :: send_msg(:,:,:)
    complex(real128),allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX32,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_c16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX32,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_c16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_c16(send_msg, pfrom, pto) result (recv_msg)
    complex(real128),intent(in) :: send_msg(:,:,:,:)
    complex(real128),allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX32,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_c16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX32,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_c16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_c16(send_msg, pfrom, pto) result (recv_msg)
    complex(real128),intent(in) :: send_msg(:,:,:,:,:)
    complex(real128),allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX32,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_c16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX32,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_c16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_c16(send_msg, pfrom, pto) result (recv_msg)
    complex(real128),intent(in) :: send_msg(:,:,:,:,:,:)
    complex(real128),allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX32,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_c16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX32,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_c16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_c16(send_msg, pfrom, pto) result (recv_msg)
    complex(real128),intent(in) :: send_msg(:,:,:,:,:,:,:)
    complex(real128),allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_COMPLEX32,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_c16: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_COMPLEX32,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_c16: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d1_l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg(:)
    logical,allocatable :: recv_msg(:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d1_l: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d2_l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg(:,:)
    logical,allocatable :: recv_msg(:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d2_l: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d3_l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg(:,:,:)
    logical,allocatable :: recv_msg(:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d3_l: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d4_l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg(:,:,:,:)
    logical,allocatable :: recv_msg(:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d4_l: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d5_l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg(:,:,:,:,:)
    logical,allocatable :: recv_msg(:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d5_l: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d6_l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg(:,:,:,:,:,:)
    logical,allocatable :: recv_msg(:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d6_l: MPI_RECV: ERROR")
    endif
    
end function
!=======================================================================
module function send_d7_l(send_msg, pfrom, pto) result (recv_msg)
    logical,intent(in) :: send_msg(:,:,:,:,:,:,:)
    logical,allocatable :: recv_msg(:,:,:,:,:,:,:)
    integer(int32),intent(in) :: pfrom, pto
    integer(int32) :: i
        
    allocate(recv_msg, source=send_msg)
    tag = bcast_mpi(tag+1,pfrom)
    
    if(this_proc == pfrom) then
        call MPI_SEND(send_msg,size(send_msg),MPI_LOGICAL,pto,tag,comm,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_l: MPI_SEND: ERROR")
    elseif(this_proc == pto) then
        call MPI_RECV(recv_msg,size(recv_msg),MPI_LOGICAL,pfrom,tag,comm,trans_stat,ierr)
        if(ierr /= 0) call stop_mpi("send_d7_l: MPI_RECV: ERROR")
    endif
    
end function
end submodule
