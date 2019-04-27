submodule (mod_mpifw) mod_broadcast_mpi

    use mpi_f08
    use iso_fortran_env

    implicit none
   
    
	
    
contains
!=======================================================================
module function broadcast_d1_i1(msg, np) result (res)
    integer(int8),intent(in) :: msg( : )
    integer(int8),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER1, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_i1: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_i1(msg, np) result (res)
    integer(int8),intent(in) :: msg( :,: )
    integer(int8),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER1, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_i1: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_i1(msg, np) result (res)
    integer(int8),intent(in) :: msg( :,:,: )
    integer(int8),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER1, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_i1: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_i1(msg, np) result (res)
    integer(int8),intent(in) :: msg( :,:,:,: )
    integer(int8),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER1, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_i1: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_i1(msg, np) result (res)
    integer(int8),intent(in) :: msg( :,:,:,:,: )
    integer(int8),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER1, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_i1: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_i1(msg, np) result (res)
    integer(int8),intent(in) :: msg( :,:,:,:,:,: )
    integer(int8),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER1, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_i1: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_i1(msg, np) result (res)
    integer(int8),intent(in) :: msg( :,:,:,:,:,:,: )
    integer(int8),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER1, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_i1: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_i2(msg, np) result (res)
    integer(int16),intent(in) :: msg( : )
    integer(int16),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER2, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_i2: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_i2(msg, np) result (res)
    integer(int16),intent(in) :: msg( :,: )
    integer(int16),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER2, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_i2: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_i2(msg, np) result (res)
    integer(int16),intent(in) :: msg( :,:,: )
    integer(int16),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER2, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_i2: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_i2(msg, np) result (res)
    integer(int16),intent(in) :: msg( :,:,:,: )
    integer(int16),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER2, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_i2: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_i2(msg, np) result (res)
    integer(int16),intent(in) :: msg( :,:,:,:,: )
    integer(int16),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER2, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_i2: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_i2(msg, np) result (res)
    integer(int16),intent(in) :: msg( :,:,:,:,:,: )
    integer(int16),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER2, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_i2: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_i2(msg, np) result (res)
    integer(int16),intent(in) :: msg( :,:,:,:,:,:,: )
    integer(int16),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER2, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_i2: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_i4(msg, np) result (res)
    integer(int32),intent(in) :: msg( : )
    integer(int32),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_i4(msg, np) result (res)
    integer(int32),intent(in) :: msg( :,: )
    integer(int32),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_i4(msg, np) result (res)
    integer(int32),intent(in) :: msg( :,:,: )
    integer(int32),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_i4(msg, np) result (res)
    integer(int32),intent(in) :: msg( :,:,:,: )
    integer(int32),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_i4(msg, np) result (res)
    integer(int32),intent(in) :: msg( :,:,:,:,: )
    integer(int32),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_i4(msg, np) result (res)
    integer(int32),intent(in) :: msg( :,:,:,:,:,: )
    integer(int32),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_i4(msg, np) result (res)
    integer(int32),intent(in) :: msg( :,:,:,:,:,:,: )
    integer(int32),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_i4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_i8(msg, np) result (res)
    integer(int64),intent(in) :: msg( : )
    integer(int64),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_i8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_i8(msg, np) result (res)
    integer(int64),intent(in) :: msg( :,: )
    integer(int64),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_i8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_i8(msg, np) result (res)
    integer(int64),intent(in) :: msg( :,:,: )
    integer(int64),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_i8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_i8(msg, np) result (res)
    integer(int64),intent(in) :: msg( :,:,:,: )
    integer(int64),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_i8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_i8(msg, np) result (res)
    integer(int64),intent(in) :: msg( :,:,:,:,: )
    integer(int64),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_i8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_i8(msg, np) result (res)
    integer(int64),intent(in) :: msg( :,:,:,:,:,: )
    integer(int64),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_i8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_i8(msg, np) result (res)
    integer(int64),intent(in) :: msg( :,:,:,:,:,:,: )
    integer(int64),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_INTEGER8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_i8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_r4(msg, np) result (res)
    real(real32),intent(in) :: msg( : )
    real(real32),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_r4(msg, np) result (res)
    real(real32),intent(in) :: msg( :,: )
    real(real32),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_r4(msg, np) result (res)
    real(real32),intent(in) :: msg( :,:,: )
    real(real32),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_r4(msg, np) result (res)
    real(real32),intent(in) :: msg( :,:,:,: )
    real(real32),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_r4(msg, np) result (res)
    real(real32),intent(in) :: msg( :,:,:,:,: )
    real(real32),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_r4(msg, np) result (res)
    real(real32),intent(in) :: msg( :,:,:,:,:,: )
    real(real32),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_r4(msg, np) result (res)
    real(real32),intent(in) :: msg( :,:,:,:,:,:,: )
    real(real32),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_r4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_r8(msg, np) result (res)
    real(real64),intent(in) :: msg( : )
    real(real64),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_r8(msg, np) result (res)
    real(real64),intent(in) :: msg( :,: )
    real(real64),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_r8(msg, np) result (res)
    real(real64),intent(in) :: msg( :,:,: )
    real(real64),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_r8(msg, np) result (res)
    real(real64),intent(in) :: msg( :,:,:,: )
    real(real64),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_r8(msg, np) result (res)
    real(real64),intent(in) :: msg( :,:,:,:,: )
    real(real64),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_r8(msg, np) result (res)
    real(real64),intent(in) :: msg( :,:,:,:,:,: )
    real(real64),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_r8(msg, np) result (res)
    real(real64),intent(in) :: msg( :,:,:,:,:,:,: )
    real(real64),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_r8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_r16(msg, np) result (res)
    real(real128),intent(in) :: msg( : )
    real(real128),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_r16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_r16(msg, np) result (res)
    real(real128),intent(in) :: msg( :,: )
    real(real128),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_r16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_r16(msg, np) result (res)
    real(real128),intent(in) :: msg( :,:,: )
    real(real128),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_r16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_r16(msg, np) result (res)
    real(real128),intent(in) :: msg( :,:,:,: )
    real(real128),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_r16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_r16(msg, np) result (res)
    real(real128),intent(in) :: msg( :,:,:,:,: )
    real(real128),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_r16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_r16(msg, np) result (res)
    real(real128),intent(in) :: msg( :,:,:,:,:,: )
    real(real128),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_r16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_r16(msg, np) result (res)
    real(real128),intent(in) :: msg( :,:,:,:,:,:,: )
    real(real128),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_REAL16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_r16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_c4(msg, np) result (res)
    complex(real32),intent(in) :: msg( : )
    complex(real32),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_c4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_c4(msg, np) result (res)
    complex(real32),intent(in) :: msg( :,: )
    complex(real32),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_c4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_c4(msg, np) result (res)
    complex(real32),intent(in) :: msg( :,:,: )
    complex(real32),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_c4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_c4(msg, np) result (res)
    complex(real32),intent(in) :: msg( :,:,:,: )
    complex(real32),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_c4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_c4(msg, np) result (res)
    complex(real32),intent(in) :: msg( :,:,:,:,: )
    complex(real32),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_c4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_c4(msg, np) result (res)
    complex(real32),intent(in) :: msg( :,:,:,:,:,: )
    complex(real32),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_c4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_c4(msg, np) result (res)
    complex(real32),intent(in) :: msg( :,:,:,:,:,:,: )
    complex(real32),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_c4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_c8(msg, np) result (res)
    complex(real64),intent(in) :: msg( : )
    complex(real64),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_c8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_c8(msg, np) result (res)
    complex(real64),intent(in) :: msg( :,: )
    complex(real64),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_c8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_c8(msg, np) result (res)
    complex(real64),intent(in) :: msg( :,:,: )
    complex(real64),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_c8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_c8(msg, np) result (res)
    complex(real64),intent(in) :: msg( :,:,:,: )
    complex(real64),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_c8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_c8(msg, np) result (res)
    complex(real64),intent(in) :: msg( :,:,:,:,: )
    complex(real64),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_c8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_c8(msg, np) result (res)
    complex(real64),intent(in) :: msg( :,:,:,:,:,: )
    complex(real64),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_c8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_c8(msg, np) result (res)
    complex(real64),intent(in) :: msg( :,:,:,:,:,:,: )
    complex(real64),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_c8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_c16(msg, np) result (res)
    complex(real128),intent(in) :: msg( : )
    complex(real128),allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX32, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_c16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_c16(msg, np) result (res)
    complex(real128),intent(in) :: msg( :,: )
    complex(real128),allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX32, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_c16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_c16(msg, np) result (res)
    complex(real128),intent(in) :: msg( :,:,: )
    complex(real128),allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX32, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_c16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_c16(msg, np) result (res)
    complex(real128),intent(in) :: msg( :,:,:,: )
    complex(real128),allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX32, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_c16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_c16(msg, np) result (res)
    complex(real128),intent(in) :: msg( :,:,:,:,: )
    complex(real128),allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX32, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_c16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_c16(msg, np) result (res)
    complex(real128),intent(in) :: msg( :,:,:,:,:,: )
    complex(real128),allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX32, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_c16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_c16(msg, np) result (res)
    complex(real128),intent(in) :: msg( :,:,:,:,:,:,: )
    complex(real128),allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_COMPLEX32, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_c16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d1_l(msg, np) result (res)
    logical,intent(in) :: msg( : )
    logical,allocatable :: res( : )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d1_l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d2_l(msg, np) result (res)
    logical,intent(in) :: msg( :,: )
    logical,allocatable :: res( :,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d2_l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d3_l(msg, np) result (res)
    logical,intent(in) :: msg( :,:,: )
    logical,allocatable :: res( :,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d3_l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d4_l(msg, np) result (res)
    logical,intent(in) :: msg( :,:,:,: )
    logical,allocatable :: res( :,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d4_l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d5_l(msg, np) result (res)
    logical,intent(in) :: msg( :,:,:,:,: )
    logical,allocatable :: res( :,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d5_l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d6_l(msg, np) result (res)
    logical,intent(in) :: msg( :,:,:,:,:,: )
    logical,allocatable :: res( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d6_l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_d7_l(msg, np) result (res)
    logical,intent(in) :: msg( :,:,:,:,:,:,: )
    logical,allocatable :: res( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    
    allocate(res, mold=msg)
    
    CALL MPI_BCAST (msg, size(msg), MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_d7_l: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_si1(msg, np) result (res)
    integer(int8),intent(in) :: msg
    integer(int8) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_INTEGER1, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_si1: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_si2(msg, np) result (res)
    integer(int16),intent(in) :: msg
    integer(int16) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_INTEGER2, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_si2: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_si4(msg, np) result (res)
    integer(int32),intent(in) :: msg
    integer(int32) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_INTEGER4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_si4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_si8(msg, np) result (res)
    integer(int64),intent(in) :: msg
    integer(int64) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_INTEGER8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_si8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_sr4(msg, np) result (res)
    real(real32),intent(in) :: msg
    real(real32) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_REAL4, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sr4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_sr8(msg, np) result (res)
    real(real64),intent(in) :: msg
    real(real64) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_REAL8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sr8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_sr16(msg, np) result (res)
    real(real128),intent(in) :: msg
    real(real128) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_REAL16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sr16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_sc4(msg, np) result (res)
    complex(real32),intent(in) :: msg
    complex(real32) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_COMPLEX8, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sc4: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_sc8(msg, np) result (res)
    complex(real64),intent(in) :: msg
    complex(real64) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_COMPLEX16, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sc8: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_sc16(msg, np) result (res)
    complex(real128),intent(in) :: msg
    complex(real128) :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_COMPLEX32, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sc16: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
!=======================================================================
module function broadcast_sl(msg, np) result (res)
    logical,intent(in) :: msg
    logical :: res
    integer(int32),intent(in) :: np
    
    CALL MPI_BCAST (msg, 1, MPI_LOGICAL, np, comm, ierr)
    if(ierr /= 0) call stop_mpi("broadcast_sl: MPI_ALLREDUCE: ERROR")
    
    res = msg
    
end function
end submodule
