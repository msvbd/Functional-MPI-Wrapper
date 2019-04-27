module mod_allreduce_mpi

    use mpi_f08
    use iso_fortran_env
	use mod_common_mpi

    implicit none
   
    private

    interface allreduce_mpi
        procedure :: allreduce_si1, allreduce_si2, allreduce_si4, allreduce_si8, &
                     allreduce_sr4, allreduce_sr8, allreduce_sr16, allreduce_sc4, &
                     allreduce_sc8, allreduce_sc16, allreduce_sl, allreduce_d1_i1, &
                     allreduce_d2_i1, allreduce_d3_i1, allreduce_d4_i1, allreduce_d5_i1, &
                     allreduce_d6_i1, allreduce_d7_i1, allreduce_d1_i2, allreduce_d2_i2, &
                     allreduce_d3_i2, allreduce_d4_i2, allreduce_d5_i2, allreduce_d6_i2, &
                     allreduce_d7_i2, allreduce_d1_i4, allreduce_d2_i4, allreduce_d3_i4, &
                     allreduce_d4_i4, allreduce_d5_i4, allreduce_d6_i4, allreduce_d7_i4, &
                     allreduce_d1_i8, allreduce_d2_i8, allreduce_d3_i8, allreduce_d4_i8, &
                     allreduce_d5_i8, allreduce_d6_i8, allreduce_d7_i8, allreduce_d1_r4, &
                     allreduce_d2_r4, allreduce_d3_r4, allreduce_d4_r4, allreduce_d5_r4, &
                     allreduce_d6_r4, allreduce_d7_r4, allreduce_d1_r8, allreduce_d2_r8, &
                     allreduce_d3_r8, allreduce_d4_r8, allreduce_d5_r8, allreduce_d6_r8, &
                     allreduce_d7_r8, allreduce_d1_r16, allreduce_d2_r16, allreduce_d3_r16, &
                     allreduce_d4_r16, allreduce_d5_r16, allreduce_d6_r16, allreduce_d7_r16, &
                     allreduce_d1_c4, allreduce_d2_c4, allreduce_d3_c4, allreduce_d4_c4, &
                     allreduce_d5_c4, allreduce_d6_c4, allreduce_d7_c4, allreduce_d1_c8, &
                     allreduce_d2_c8, allreduce_d3_c8, allreduce_d4_c8, allreduce_d5_c8, &
                     allreduce_d6_c8, allreduce_d7_c8, allreduce_d1_c16, allreduce_d2_c16, &
                     allreduce_d3_c16, allreduce_d4_c16, allreduce_d5_c16, allreduce_d6_c16, &
                     allreduce_d7_c16, allreduce_d1_l, allreduce_d2_l, allreduce_d3_l, &
                     allreduce_d4_l, allreduce_d5_l, allreduce_d6_l, allreduce_d7_l
    end interface
    
    public :: allreduce_mpi
              
    
contains
!=======================================================================
function allreduce_si1(msg, op) result (res)
    integer(int8),intent(in) :: msg
    character(len=*),intent(in) :: op
    integer(int8) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_INTEGER1, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_si1: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_si2(msg, op) result (res)
    integer(int16),intent(in) :: msg
    character(len=*),intent(in) :: op
    integer(int16) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_INTEGER2, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_si2: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_si4(msg, op) result (res)
    integer(int32),intent(in) :: msg
    character(len=*),intent(in) :: op
    integer(int32) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_INTEGER4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_si4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_si8(msg, op) result (res)
    integer(int64),intent(in) :: msg
    character(len=*),intent(in) :: op
    integer(int64) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_INTEGER8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_si8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sr4(msg, op) result (res)
    real(real32),intent(in) :: msg
    character(len=*),intent(in) :: op
    real(real32) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sr4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sr8(msg, op) result (res)
    real(real64),intent(in) :: msg
    character(len=*),intent(in) :: op
    real(real64) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sr8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sr16(msg, op) result (res)
    real(real128),intent(in) :: msg
    character(len=*),intent(in) :: op
    real(real128) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_REAL16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sr16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sc4(msg, op) result (res)
    complex(real32),intent(in) :: msg
    character(len=*),intent(in) :: op
    complex(real32) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_COMPLEX8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sc4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sc8(msg, op) result (res)
    complex(real64),intent(in) :: msg
    character(len=*),intent(in) :: op
    complex(real64) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_COMPLEX16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sc8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sc16(msg, op) result (res)
    complex(real128),intent(in) :: msg
    character(len=*),intent(in) :: op
    complex(real128) :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_COMPLEX32, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sc16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_sl(msg, op) result (res)
    logical,intent(in) :: msg
    character(len=*),intent(in) :: op
    logical :: res
    
    CALL MPI_ALLREDUCE (msg, res, 1, MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_sl: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_i1(msg, op) result (res)
    integer(int8),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER1, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_i1: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_i1(msg, op) result (res)
    integer(int8),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER1, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_i1: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_i1(msg, op) result (res)
    integer(int8),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER1, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_i1: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_i1(msg, op) result (res)
    integer(int8),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER1, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_i1: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_i1(msg, op) result (res)
    integer(int8),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER1, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_i1: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_i1(msg, op) result (res)
    integer(int8),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER1, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_i1: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_i1(msg, op) result (res)
    integer(int8),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int8),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER1, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_i1: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_i2(msg, op) result (res)
    integer(int16),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER2, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_i2: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_i2(msg, op) result (res)
    integer(int16),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER2, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_i2: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_i2(msg, op) result (res)
    integer(int16),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER2, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_i2: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_i2(msg, op) result (res)
    integer(int16),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER2, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_i2: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_i2(msg, op) result (res)
    integer(int16),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER2, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_i2: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_i2(msg, op) result (res)
    integer(int16),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER2, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_i2: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_i2(msg, op) result (res)
    integer(int16),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int16),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER2, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_i2: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_i4(msg, op) result (res)
    integer(int32),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_i4(msg, op) result (res)
    integer(int32),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_i4(msg, op) result (res)
    integer(int32),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_i4(msg, op) result (res)
    integer(int32),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_i4(msg, op) result (res)
    integer(int32),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_i4(msg, op) result (res)
    integer(int32),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_i4(msg, op) result (res)
    integer(int32),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int32),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_i4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_i8(msg, op) result (res)
    integer(int64),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_i8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_i8(msg, op) result (res)
    integer(int64),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_i8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_i8(msg, op) result (res)
    integer(int64),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_i8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_i8(msg, op) result (res)
    integer(int64),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_i8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_i8(msg, op) result (res)
    integer(int64),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_i8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_i8(msg, op) result (res)
    integer(int64),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_i8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_i8(msg, op) result (res)
    integer(int64),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    integer(int64),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_INTEGER8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_i8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_r4(msg, op) result (res)
    real(real32),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_r4(msg, op) result (res)
    real(real32),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_r4(msg, op) result (res)
    real(real32),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_r4(msg, op) result (res)
    real(real32),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_r4(msg, op) result (res)
    real(real32),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_r4(msg, op) result (res)
    real(real32),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_r4(msg, op) result (res)
    real(real32),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    real(real32),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL4, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_r4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_r8(msg, op) result (res)
    real(real64),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_r8(msg, op) result (res)
    real(real64),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_r8(msg, op) result (res)
    real(real64),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_r8(msg, op) result (res)
    real(real64),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_r8(msg, op) result (res)
    real(real64),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_r8(msg, op) result (res)
    real(real64),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_r8(msg, op) result (res)
    real(real64),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    real(real64),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_r8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_r16(msg, op) result (res)
    real(real128),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_r16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_r16(msg, op) result (res)
    real(real128),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_r16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_r16(msg, op) result (res)
    real(real128),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_r16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_r16(msg, op) result (res)
    real(real128),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_r16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_r16(msg, op) result (res)
    real(real128),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_r16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_r16(msg, op) result (res)
    real(real128),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_r16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_r16(msg, op) result (res)
    real(real128),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    real(real128),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_REAL16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_r16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_c4(msg, op) result (res)
    complex(real32),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_c4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_c4(msg, op) result (res)
    complex(real32),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_c4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_c4(msg, op) result (res)
    complex(real32),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_c4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_c4(msg, op) result (res)
    complex(real32),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_c4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_c4(msg, op) result (res)
    complex(real32),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_c4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_c4(msg, op) result (res)
    complex(real32),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_c4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_c4(msg, op) result (res)
    complex(real32),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    complex(real32),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX8, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_c4: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_c8(msg, op) result (res)
    complex(real64),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_c8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_c8(msg, op) result (res)
    complex(real64),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_c8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_c8(msg, op) result (res)
    complex(real64),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_c8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_c8(msg, op) result (res)
    complex(real64),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_c8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_c8(msg, op) result (res)
    complex(real64),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_c8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_c8(msg, op) result (res)
    complex(real64),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_c8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_c8(msg, op) result (res)
    complex(real64),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    complex(real64),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX16, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_c8: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_c16(msg, op) result (res)
    complex(real128),intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX32, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_c16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_c16(msg, op) result (res)
    complex(real128),intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX32, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_c16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_c16(msg, op) result (res)
    complex(real128),intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX32, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_c16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_c16(msg, op) result (res)
    complex(real128),intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX32, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_c16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_c16(msg, op) result (res)
    complex(real128),intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX32, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_c16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_c16(msg, op) result (res)
    complex(real128),intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX32, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_c16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_c16(msg, op) result (res)
    complex(real128),intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    complex(real128),allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_COMPLEX32, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_c16: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d1_l(msg, op) result (res)
    logical,intent(in) :: msg( : )
    character(len=*),intent(in) :: op
    logical,allocatable :: res( : )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d1_l: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d2_l(msg, op) result (res)
    logical,intent(in) :: msg( :,: )
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d2_l: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d3_l(msg, op) result (res)
    logical,intent(in) :: msg( :,:,: )
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d3_l: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d4_l(msg, op) result (res)
    logical,intent(in) :: msg( :,:,:,: )
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d4_l: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d5_l(msg, op) result (res)
    logical,intent(in) :: msg( :,:,:,:,: )
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d5_l: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d6_l(msg, op) result (res)
    logical,intent(in) :: msg( :,:,:,:,:,: )
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d6_l: MPI_ALLREDUCE: ERROR")
    
end function
!=======================================================================
function allreduce_d7_l(msg, op) result (res)
    logical,intent(in) :: msg( :,:,:,:,:,:,: )
    character(len=*),intent(in) :: op
    logical,allocatable :: res( :,:,:,:,:,:,: )
    
    allocate(res, mold=msg)
    
    CALL MPI_ALLREDUCE (msg, res, size(msg), MPI_LOGICAL, CollOp(op), comm, ierr)
    if(ierr /= 0) call stop_mpi("allreduce_d7_l: MPI_ALLREDUCE: ERROR")
    
end function
end module
