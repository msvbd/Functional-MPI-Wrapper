submodule (mod_mpifw) mod_reduce_mpi

    use mpi_f08
    use iso_fortran_env

    implicit none
    
    
	
              
contains
!=======================================================================
module function reduce_si1(what, np, op) result (res)
    integer(int8),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int8) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_INTEGER1, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_si1: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_si2(what, np, op) result (res)
    integer(int16),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int16) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_INTEGER2, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_si2: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_si4(what, np, op) result (res)
    integer(int32),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_INTEGER4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_si4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_si8(what, np, op) result (res)
    integer(int64),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int64) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_INTEGER8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_si8: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_sr4(what, np, op) result (res)
    real(real32),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_sr4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_sr8(what, np, op) result (res)
    real(real64),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_sr8: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_sr16(what, np, op) result (res)
    real(real128),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real128) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_REAL16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_sr16: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_sc4(what, np, op) result (res)
    complex(real32),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real32) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_COMPLEX8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_sc4: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_sc8(what, np, op) result (res)
    complex(real64),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real64) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_COMPLEX16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_sc8: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_sc16(what, np, op) result (res)
    complex(real128),intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real128) :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_COMPLEX32, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_sc16: MPI_REDUCE: ERROR")
    
end function
!=======================================================================
module function reduce_sl(what, np, op) result (res)
    logical,intent(in) :: what
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical :: res
    
    res = what
    
    CALL MPI_REDUCE (what, res, 1, MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("rreduce_sl: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_i1(what, np, op) result (res)
    integer(int8),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER1, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_i1: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_i1(what, np, op) result (res)
    integer(int8),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER1, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_i1: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_i1(what, np, op) result (res)
    integer(int8),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER1, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_i1: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_i1(what, np, op) result (res)
    integer(int8),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER1, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_i1: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_i1(what, np, op) result (res)
    integer(int8),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER1, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_i1: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_i1(what, np, op) result (res)
    integer(int8),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER1, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_i1: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_i1(what, np, op) result (res)
    integer(int8),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER1, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_i1: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_i2(what, np, op) result (res)
    integer(int16),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER2, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_i2: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_i2(what, np, op) result (res)
    integer(int16),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER2, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_i2: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_i2(what, np, op) result (res)
    integer(int16),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER2, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_i2: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_i2(what, np, op) result (res)
    integer(int16),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER2, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_i2: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_i2(what, np, op) result (res)
    integer(int16),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER2, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_i2: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_i2(what, np, op) result (res)
    integer(int16),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER2, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_i2: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_i2(what, np, op) result (res)
    integer(int16),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER2, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_i2: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_i4(what, np, op) result (res)
    integer(int32),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_i4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_i4(what, np, op) result (res)
    integer(int32),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_i4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_i4(what, np, op) result (res)
    integer(int32),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_i4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_i4(what, np, op) result (res)
    integer(int32),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_i4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_i4(what, np, op) result (res)
    integer(int32),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_i4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_i4(what, np, op) result (res)
    integer(int32),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_i4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_i4(what, np, op) result (res)
    integer(int32),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_i4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_i8(what, np, op) result (res)
    integer(int64),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_i8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_i8(what, np, op) result (res)
    integer(int64),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_i8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_i8(what, np, op) result (res)
    integer(int64),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_i8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_i8(what, np, op) result (res)
    integer(int64),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_i8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_i8(what, np, op) result (res)
    integer(int64),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_i8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_i8(what, np, op) result (res)
    integer(int64),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_i8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_i8(what, np, op) result (res)
    integer(int64),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_INTEGER8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_i8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_r4(what, np, op) result (res)
    real(real32),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_r4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_r4(what, np, op) result (res)
    real(real32),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_r4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_r4(what, np, op) result (res)
    real(real32),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_r4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_r4(what, np, op) result (res)
    real(real32),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_r4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_r4(what, np, op) result (res)
    real(real32),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_r4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_r4(what, np, op) result (res)
    real(real32),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_r4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_r4(what, np, op) result (res)
    real(real32),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL4, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_r4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_r8(what, np, op) result (res)
    real(real64),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_r8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_r8(what, np, op) result (res)
    real(real64),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_r8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_r8(what, np, op) result (res)
    real(real64),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_r8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_r8(what, np, op) result (res)
    real(real64),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_r8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_r8(what, np, op) result (res)
    real(real64),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_r8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_r8(what, np, op) result (res)
    real(real64),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_r8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_r8(what, np, op) result (res)
    real(real64),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_r8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_r16(what, np, op) result (res)
    real(real128),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_r16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_r16(what, np, op) result (res)
    real(real128),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_r16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_r16(what, np, op) result (res)
    real(real128),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_r16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_r16(what, np, op) result (res)
    real(real128),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_r16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_r16(what, np, op) result (res)
    real(real128),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_r16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_r16(what, np, op) result (res)
    real(real128),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_r16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_r16(what, np, op) result (res)
    real(real128),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_REAL16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_r16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_c4(what, np, op) result (res)
    complex(real32),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_c4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_c4(what, np, op) result (res)
    complex(real32),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_c4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_c4(what, np, op) result (res)
    complex(real32),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_c4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_c4(what, np, op) result (res)
    complex(real32),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_c4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_c4(what, np, op) result (res)
    complex(real32),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_c4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_c4(what, np, op) result (res)
    complex(real32),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_c4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_c4(what, np, op) result (res)
    complex(real32),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX8, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_c4: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_c8(what, np, op) result (res)
    complex(real64),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_c8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_c8(what, np, op) result (res)
    complex(real64),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_c8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_c8(what, np, op) result (res)
    complex(real64),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_c8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_c8(what, np, op) result (res)
    complex(real64),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_c8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_c8(what, np, op) result (res)
    complex(real64),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_c8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_c8(what, np, op) result (res)
    complex(real64),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_c8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_c8(what, np, op) result (res)
    complex(real64),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX16, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_c8: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_c16(what, np, op) result (res)
    complex(real128),intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX32, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_c16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_c16(what, np, op) result (res)
    complex(real128),intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX32, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_c16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_c16(what, np, op) result (res)
    complex(real128),intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX32, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_c16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_c16(what, np, op) result (res)
    complex(real128),intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX32, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_c16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_c16(what, np, op) result (res)
    complex(real128),intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX32, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_c16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_c16(what, np, op) result (res)
    complex(real128),intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX32, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_c16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_c16(what, np, op) result (res)
    complex(real128),intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_COMPLEX32, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_c16: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d1_l(what, np, op) result (res)
    logical,intent(in) :: what( : )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical,allocatable :: res( : )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d1_l: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d2_l(what, np, op) result (res)
    logical,intent(in) :: what( :,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d2_l: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d3_l(what, np, op) result (res)
    logical,intent(in) :: what( :,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d3_l: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d4_l(what, np, op) result (res)
    logical,intent(in) :: what( :,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d4_l: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d5_l(what, np, op) result (res)
    logical,intent(in) :: what( :,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d5_l: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d6_l(what, np, op) result (res)
    logical,intent(in) :: what( :,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d6_l: MPI_REDUCE: ERROR")
    
end function
!======================================================================
module function reduce_d7_l(what, np, op) result (res)
    logical,intent(in) :: what( :,:,:,:,:,:,: )
    integer(int32),intent(in) :: np
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=what)
    res = what
    
    CALL MPI_REDUCE (what, res, size(what), MPI_LOGICAL, CollOp(op), np, comm, ierr)
    if(ierr /= 0) call stop_mpi("reduce_d7_l: MPI_REDUCE: ERROR")
    
end function
end submodule
