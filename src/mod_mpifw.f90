module mod_mpifw
    use iso_fortran_env
    use mpi_f08
    implicit none
    
    private
     
    interface
		module function reduce_si1(what, np, op) result (res)
			integer(int8),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int8) :: res
		end function
		module function reduce_si2(what, np, op) result (res)
			integer(int16),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int16) :: res
		end function
		module function reduce_si4(what, np, op) result (res)
			integer(int32),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int32) :: res
		end function
		module function reduce_si8(what, np, op) result (res)
			integer(int64),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int64) :: res
		end function
		module function reduce_sr4(what, np, op) result (res)
			real(real32),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real32) :: res
		end function
		module function reduce_sr8(what, np, op) result (res)
			real(real64),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real64) :: res
		end function
		module function reduce_sr16(what, np, op) result (res)
			real(real128),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real128) :: res
		end function
		module function reduce_sc4(what, np, op) result (res)
			complex(real32),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real32) :: res
		end function
		module function reduce_sc8(what, np, op) result (res)
			complex(real64),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real64) :: res
		end function
		module function reduce_sc16(what, np, op) result (res)
			complex(real128),intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real128) :: res
		end function
		module function reduce_sl(what, np, op) result (res)
			logical,intent(in) :: what
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			logical :: res
		end function
		module function reduce_d1_i1(what, np, op) result (res)
			integer(int8),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( : )
		end function
		module function reduce_d2_i1(what, np, op) result (res)
			integer(int8),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,: )
		end function
		module function reduce_d3_i1(what, np, op) result (res)
			integer(int8),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_i1(what, np, op) result (res)
			integer(int8),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_i1(what, np, op) result (res)
			integer(int8),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_i1(what, np, op) result (res)
			integer(int8),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_i1(what, np, op) result (res)
			integer(int8),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_i2(what, np, op) result (res)
			integer(int16),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( : )
		end function
		module function reduce_d2_i2(what, np, op) result (res)
			integer(int16),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,: )
		end function
		module function reduce_d3_i2(what, np, op) result (res)
			integer(int16),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_i2(what, np, op) result (res)
			integer(int16),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_i2(what, np, op) result (res)
			integer(int16),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_i2(what, np, op) result (res)
			integer(int16),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_i2(what, np, op) result (res)
			integer(int16),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_i4(what, np, op) result (res)
			integer(int32),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( : )
		end function
		module function reduce_d2_i4(what, np, op) result (res)
			integer(int32),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,: )
		end function
		module function reduce_d3_i4(what, np, op) result (res)
			integer(int32),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_i4(what, np, op) result (res)
			integer(int32),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_i4(what, np, op) result (res)
			integer(int32),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_i4(what, np, op) result (res)
			integer(int32),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_i4(what, np, op) result (res)
			integer(int32),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_i8(what, np, op) result (res)
			integer(int64),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( : )
		end function
		module function reduce_d2_i8(what, np, op) result (res)
			integer(int64),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,: )
		end function
		module function reduce_d3_i8(what, np, op) result (res)
			integer(int64),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_i8(what, np, op) result (res)
			integer(int64),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_i8(what, np, op) result (res)
			integer(int64),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_i8(what, np, op) result (res)
			integer(int64),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_i8(what, np, op) result (res)
			integer(int64),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_r4(what, np, op) result (res)
			real(real32),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( : )
		end function
		module function reduce_d2_r4(what, np, op) result (res)
			real(real32),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,: )
		end function
		module function reduce_d3_r4(what, np, op) result (res)
			real(real32),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_r4(what, np, op) result (res)
			real(real32),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_r4(what, np, op) result (res)
			real(real32),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_r4(what, np, op) result (res)
			real(real32),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_r4(what, np, op) result (res)
			real(real32),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_r8(what, np, op) result (res)
			real(real64),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( : )
		end function
		module function reduce_d2_r8(what, np, op) result (res)
			real(real64),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,: )
		end function
		module function reduce_d3_r8(what, np, op) result (res)
			real(real64),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_r8(what, np, op) result (res)
			real(real64),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_r8(what, np, op) result (res)
			real(real64),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_r8(what, np, op) result (res)
			real(real64),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_r8(what, np, op) result (res)
			real(real64),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_r16(what, np, op) result (res)
			real(real128),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( : )
		end function
		module function reduce_d2_r16(what, np, op) result (res)
			real(real128),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,: )
		end function
		module function reduce_d3_r16(what, np, op) result (res)
			real(real128),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_r16(what, np, op) result (res)
			real(real128),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_r16(what, np, op) result (res)
			real(real128),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_r16(what, np, op) result (res)
			real(real128),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_r16(what, np, op) result (res)
			real(real128),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_c4(what, np, op) result (res)
			complex(real32),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( : )
		end function
		module function reduce_d2_c4(what, np, op) result (res)
			complex(real32),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,: )
		end function
		module function reduce_d3_c4(what, np, op) result (res)
			complex(real32),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_c4(what, np, op) result (res)
			complex(real32),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_c4(what, np, op) result (res)
			complex(real32),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_c4(what, np, op) result (res)
			complex(real32),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_c4(what, np, op) result (res)
			complex(real32),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_c8(what, np, op) result (res)
			complex(real64),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( : )
		end function
		module function reduce_d2_c8(what, np, op) result (res)
			complex(real64),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,: )
		end function
		module function reduce_d3_c8(what, np, op) result (res)
			complex(real64),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_c8(what, np, op) result (res)
			complex(real64),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_c8(what, np, op) result (res)
			complex(real64),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_c8(what, np, op) result (res)
			complex(real64),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_c8(what, np, op) result (res)
			complex(real64),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_c16(what, np, op) result (res)
			complex(real128),intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( : )
		end function
		module function reduce_d2_c16(what, np, op) result (res)
			complex(real128),intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,: )
		end function
		module function reduce_d3_c16(what, np, op) result (res)
			complex(real128),intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,: )
		end function
		module function reduce_d4_c16(what, np, op) result (res)
			complex(real128),intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_c16(what, np, op) result (res)
			complex(real128),intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_c16(what, np, op) result (res)
			complex(real128),intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_c16(what, np, op) result (res)
			complex(real128),intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function reduce_d1_l(what, np, op) result (res)
			logical,intent(in) :: what( : )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			logical,allocatable :: res( : )
		end function
		module function reduce_d2_l(what, np, op) result (res)
			logical,intent(in) :: what( :,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,: )
		end function
		module function reduce_d3_l(what, np, op) result (res)
			logical,intent(in) :: what( :,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,: )
		end function
		module function reduce_d4_l(what, np, op) result (res)
			logical,intent(in) :: what( :,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,:,: )
		end function
		module function reduce_d5_l(what, np, op) result (res)
			logical,intent(in) :: what( :,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,:,:,: )
		end function
		module function reduce_d6_l(what, np, op) result (res)
			logical,intent(in) :: what( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,:,:,:,: )
		end function
		module function reduce_d7_l(what, np, op) result (res)
			logical,intent(in) :: what( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,:,:,:,:,: )
		end function
		module subroutine sync_mpi()
		end subroutine
		module subroutine stop_mpi(msg)
			character(len=*),optional :: msg
		end subroutine
		module function CollOp(op) result (co)
			!import :: MPI_Op
			character(len=*), intent(in) :: op
			 type(MPI_Op) :: co
		end function
    module function allreduce_si1(msg, op) result (res)
		integer(int8),intent(in) :: msg
		character(len=*),intent(in) :: op
		integer(int8) :: res
    end function
    module function allreduce_si2(msg, op) result (res)
		integer(int16),intent(in) :: msg
		character(len=*),intent(in) :: op
		integer(int16) :: res
    end function
    module function allreduce_si4(msg, op) result (res)
		integer(int32),intent(in) :: msg
		character(len=*),intent(in) :: op
		integer(int32) :: res
    end function
    module function allreduce_si8(msg, op) result (res)
		integer(int64),intent(in) :: msg
		character(len=*),intent(in) :: op
		integer(int64) :: res
    end function
    module function allreduce_sr4(msg, op) result (res)
		real(real32),intent(in) :: msg
		character(len=*),intent(in) :: op
		real(real32) :: res
    end function
    module function allreduce_sr8(msg, op) result (res)
		real(real64),intent(in) :: msg
		character(len=*),intent(in) :: op
		real(real64) :: res
    end function
    module function allreduce_sr16(msg, op) result (res)
		real(real128),intent(in) :: msg
		character(len=*),intent(in) :: op
		real(real128) :: res
    end function
    module function allreduce_sc4(msg, op) result (res)
		complex(real32),intent(in) :: msg
		character(len=*),intent(in) :: op
		complex(real32) :: res
    end function
    module function allreduce_sc8(msg, op) result (res)
		complex(real64),intent(in) :: msg
		character(len=*),intent(in) :: op
		complex(real64) :: res
    end function
    module function allreduce_sc16(msg, op) result (res)
		complex(real128),intent(in) :: msg
		character(len=*),intent(in) :: op
		complex(real128) :: res
    end function
    module function allreduce_sl(msg, op) result (res)
		logical,intent(in) :: msg
		character(len=*),intent(in) :: op
		logical :: res
    end function
		module function allreduce_d1_i1(msg, op) result (res)
			integer(int8),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( : )
		end function
		module function allreduce_d2_i1(msg, op) result (res)
			integer(int8),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,: )
		end function
		module function allreduce_d3_i1(msg, op) result (res)
			integer(int8),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_i1(msg, op) result (res)
			integer(int8),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_i1(msg, op) result (res)
			integer(int8),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_i1(msg, op) result (res)
			integer(int8),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_i1(msg, op) result (res)
			integer(int8),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int8),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_i2(msg, op) result (res)
			integer(int16),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( : )
		end function
		module function allreduce_d2_i2(msg, op) result (res)
			integer(int16),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,: )
		end function
		module function allreduce_d3_i2(msg, op) result (res)
			integer(int16),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_i2(msg, op) result (res)
			integer(int16),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_i2(msg, op) result (res)
			integer(int16),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_i2(msg, op) result (res)
			integer(int16),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_i2(msg, op) result (res)
			integer(int16),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int16),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_i4(msg, op) result (res)
			integer(int32),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( : )
		end function
		module function allreduce_d2_i4(msg, op) result (res)
			integer(int32),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,: )
		end function
		module function allreduce_d3_i4(msg, op) result (res)
			integer(int32),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_i4(msg, op) result (res)
			integer(int32),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_i4(msg, op) result (res)
			integer(int32),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_i4(msg, op) result (res)
			integer(int32),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_i4(msg, op) result (res)
			integer(int32),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int32),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_i8(msg, op) result (res)
			integer(int64),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( : )
		end function
		module function allreduce_d2_i8(msg, op) result (res)
			integer(int64),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,: )
		end function
		module function allreduce_d3_i8(msg, op) result (res)
			integer(int64),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_i8(msg, op) result (res)
			integer(int64),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_i8(msg, op) result (res)
			integer(int64),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_i8(msg, op) result (res)
			integer(int64),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_i8(msg, op) result (res)
			integer(int64),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			integer(int64),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_r4(msg, op) result (res)
			real(real32),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( : )
		end function
		module function allreduce_d2_r4(msg, op) result (res)
			real(real32),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,: )
		end function
		module function allreduce_d3_r4(msg, op) result (res)
			real(real32),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_r4(msg, op) result (res)
			real(real32),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_r4(msg, op) result (res)
			real(real32),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_r4(msg, op) result (res)
			real(real32),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_r4(msg, op) result (res)
			real(real32),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			real(real32),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_r8(msg, op) result (res)
			real(real64),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( : )
		end function
		module function allreduce_d2_r8(msg, op) result (res)
			real(real64),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,: )
		end function
		module function allreduce_d3_r8(msg, op) result (res)
			real(real64),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_r8(msg, op) result (res)
			real(real64),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_r8(msg, op) result (res)
			real(real64),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_r8(msg, op) result (res)
			real(real64),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_r8(msg, op) result (res)
			real(real64),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			real(real64),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_r16(msg, op) result (res)
			real(real128),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( : )
		end function
		module function allreduce_d2_r16(msg, op) result (res)
			real(real128),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,: )
		end function
		module function allreduce_d3_r16(msg, op) result (res)
			real(real128),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_r16(msg, op) result (res)
			real(real128),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_r16(msg, op) result (res)
			real(real128),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_r16(msg, op) result (res)
			real(real128),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_r16(msg, op) result (res)
			real(real128),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			real(real128),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_c4(msg, op) result (res)
			complex(real32),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( : )
		end function
		module function allreduce_d2_c4(msg, op) result (res)
			complex(real32),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,: )
		end function
		module function allreduce_d3_c4(msg, op) result (res)
			complex(real32),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_c4(msg, op) result (res)
			complex(real32),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_c4(msg, op) result (res)
			complex(real32),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_c4(msg, op) result (res)
			complex(real32),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_c4(msg, op) result (res)
			complex(real32),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			complex(real32),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_c8(msg, op) result (res)
			complex(real64),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( : )
		end function
		module function allreduce_d2_c8(msg, op) result (res)
			complex(real64),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,: )
		end function
		module function allreduce_d3_c8(msg, op) result (res)
			complex(real64),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_c8(msg, op) result (res)
			complex(real64),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_c8(msg, op) result (res)
			complex(real64),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_c8(msg, op) result (res)
			complex(real64),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_c8(msg, op) result (res)
			complex(real64),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			complex(real64),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_c16(msg, op) result (res)
			complex(real128),intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( : )
		end function
		module function allreduce_d2_c16(msg, op) result (res)
			complex(real128),intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,: )
		end function
		module function allreduce_d3_c16(msg, op) result (res)
			complex(real128),intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_c16(msg, op) result (res)
			complex(real128),intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_c16(msg, op) result (res)
			complex(real128),intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_c16(msg, op) result (res)
			complex(real128),intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_c16(msg, op) result (res)
			complex(real128),intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			complex(real128),allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function allreduce_d1_l(msg, op) result (res)
			logical,intent(in) :: msg( : )
			character(len=*),intent(in) :: op
			logical,allocatable :: res( : )
		end function
		module function allreduce_d2_l(msg, op) result (res)
			logical,intent(in) :: msg( :,: )
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,: )
		end function
		module function allreduce_d3_l(msg, op) result (res)
			logical,intent(in) :: msg( :,:,: )
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,: )
		end function
		module function allreduce_d4_l(msg, op) result (res)
			logical,intent(in) :: msg( :,:,:,: )
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,:,: )
		end function
		module function allreduce_d5_l(msg, op) result (res)
			logical,intent(in) :: msg( :,:,:,:,: )
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,:,:,: )
		end function
		module function allreduce_d6_l(msg, op) result (res)
			logical,intent(in) :: msg( :,:,:,:,:,: )
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,:,:,:,: )
		end function
		module function allreduce_d7_l(msg, op) result (res)
			logical,intent(in) :: msg( :,:,:,:,:,:,: )
			character(len=*),intent(in) :: op
			logical,allocatable :: res( :,:,:,:,:,:,: )
		end function
		module function broadcast_d1_i1(msg, np) result (res)
			integer(int8),intent(in) :: msg( : )
			integer(int8),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_i1(msg, np) result (res)
			integer(int8),intent(in) :: msg( :,: )
			integer(int8),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_i1(msg, np) result (res)
			integer(int8),intent(in) :: msg( :,:,: )
			integer(int8),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_i1(msg, np) result (res)
			integer(int8),intent(in) :: msg( :,:,:,: )
			integer(int8),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_i1(msg, np) result (res)
			integer(int8),intent(in) :: msg( :,:,:,:,: )
			integer(int8),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_i1(msg, np) result (res)
			integer(int8),intent(in) :: msg( :,:,:,:,:,: )
			integer(int8),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_i1(msg, np) result (res)
			integer(int8),intent(in) :: msg( :,:,:,:,:,:,: )
			integer(int8),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_i2(msg, np) result (res)
			integer(int16),intent(in) :: msg( : )
			integer(int16),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_i2(msg, np) result (res)
			integer(int16),intent(in) :: msg( :,: )
			integer(int16),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_i2(msg, np) result (res)
			integer(int16),intent(in) :: msg( :,:,: )
			integer(int16),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_i2(msg, np) result (res)
			integer(int16),intent(in) :: msg( :,:,:,: )
			integer(int16),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_i2(msg, np) result (res)
			integer(int16),intent(in) :: msg( :,:,:,:,: )
			integer(int16),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_i2(msg, np) result (res)
			integer(int16),intent(in) :: msg( :,:,:,:,:,: )
			integer(int16),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_i2(msg, np) result (res)
			integer(int16),intent(in) :: msg( :,:,:,:,:,:,: )
			integer(int16),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_i4(msg, np) result (res)
			integer(int32),intent(in) :: msg( : )
			integer(int32),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_i4(msg, np) result (res)
			integer(int32),intent(in) :: msg( :,: )
			integer(int32),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_i4(msg, np) result (res)
			integer(int32),intent(in) :: msg( :,:,: )
			integer(int32),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_i4(msg, np) result (res)
			integer(int32),intent(in) :: msg( :,:,:,: )
			integer(int32),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_i4(msg, np) result (res)
			integer(int32),intent(in) :: msg( :,:,:,:,: )
			integer(int32),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_i4(msg, np) result (res)
			integer(int32),intent(in) :: msg( :,:,:,:,:,: )
			integer(int32),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_i4(msg, np) result (res)
			integer(int32),intent(in) :: msg( :,:,:,:,:,:,: )
			integer(int32),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_i8(msg, np) result (res)
			integer(int64),intent(in) :: msg( : )
			integer(int64),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_i8(msg, np) result (res)
			integer(int64),intent(in) :: msg( :,: )
			integer(int64),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_i8(msg, np) result (res)
			integer(int64),intent(in) :: msg( :,:,: )
			integer(int64),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_i8(msg, np) result (res)
			integer(int64),intent(in) :: msg( :,:,:,: )
			integer(int64),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_i8(msg, np) result (res)
			integer(int64),intent(in) :: msg( :,:,:,:,: )
			integer(int64),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_i8(msg, np) result (res)
			integer(int64),intent(in) :: msg( :,:,:,:,:,: )
			integer(int64),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_i8(msg, np) result (res)
			integer(int64),intent(in) :: msg( :,:,:,:,:,:,: )
			integer(int64),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_r4(msg, np) result (res)
			real(real32),intent(in) :: msg( : )
			real(real32),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_r4(msg, np) result (res)
			real(real32),intent(in) :: msg( :,: )
			real(real32),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_r4(msg, np) result (res)
			real(real32),intent(in) :: msg( :,:,: )
			real(real32),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_r4(msg, np) result (res)
			real(real32),intent(in) :: msg( :,:,:,: )
			real(real32),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_r4(msg, np) result (res)
			real(real32),intent(in) :: msg( :,:,:,:,: )
			real(real32),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_r4(msg, np) result (res)
			real(real32),intent(in) :: msg( :,:,:,:,:,: )
			real(real32),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_r4(msg, np) result (res)
			real(real32),intent(in) :: msg( :,:,:,:,:,:,: )
			real(real32),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_r8(msg, np) result (res)
			real(real64),intent(in) :: msg( : )
			real(real64),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_r8(msg, np) result (res)
			real(real64),intent(in) :: msg( :,: )
			real(real64),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_r8(msg, np) result (res)
			real(real64),intent(in) :: msg( :,:,: )
			real(real64),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_r8(msg, np) result (res)
			real(real64),intent(in) :: msg( :,:,:,: )
			real(real64),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_r8(msg, np) result (res)
			real(real64),intent(in) :: msg( :,:,:,:,: )
			real(real64),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_r8(msg, np) result (res)
			real(real64),intent(in) :: msg( :,:,:,:,:,: )
			real(real64),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_r8(msg, np) result (res)
			real(real64),intent(in) :: msg( :,:,:,:,:,:,: )
			real(real64),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_r16(msg, np) result (res)
			real(real128),intent(in) :: msg( : )
			real(real128),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_r16(msg, np) result (res)
			real(real128),intent(in) :: msg( :,: )
			real(real128),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_r16(msg, np) result (res)
			real(real128),intent(in) :: msg( :,:,: )
			real(real128),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_r16(msg, np) result (res)
			real(real128),intent(in) :: msg( :,:,:,: )
			real(real128),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_r16(msg, np) result (res)
			real(real128),intent(in) :: msg( :,:,:,:,: )
			real(real128),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_r16(msg, np) result (res)
			real(real128),intent(in) :: msg( :,:,:,:,:,: )
			real(real128),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_r16(msg, np) result (res)
			real(real128),intent(in) :: msg( :,:,:,:,:,:,: )
			real(real128),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_c4(msg, np) result (res)
			complex(real32),intent(in) :: msg( : )
			complex(real32),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_c4(msg, np) result (res)
			complex(real32),intent(in) :: msg( :,: )
			complex(real32),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_c4(msg, np) result (res)
			complex(real32),intent(in) :: msg( :,:,: )
			complex(real32),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_c4(msg, np) result (res)
			complex(real32),intent(in) :: msg( :,:,:,: )
			complex(real32),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_c4(msg, np) result (res)
			complex(real32),intent(in) :: msg( :,:,:,:,: )
			complex(real32),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_c4(msg, np) result (res)
			complex(real32),intent(in) :: msg( :,:,:,:,:,: )
			complex(real32),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_c4(msg, np) result (res)
			complex(real32),intent(in) :: msg( :,:,:,:,:,:,: )
			complex(real32),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_c8(msg, np) result (res)
			complex(real64),intent(in) :: msg( : )
			complex(real64),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_c8(msg, np) result (res)
			complex(real64),intent(in) :: msg( :,: )
			complex(real64),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_c8(msg, np) result (res)
			complex(real64),intent(in) :: msg( :,:,: )
			complex(real64),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_c8(msg, np) result (res)
			complex(real64),intent(in) :: msg( :,:,:,: )
			complex(real64),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_c8(msg, np) result (res)
			complex(real64),intent(in) :: msg( :,:,:,:,: )
			complex(real64),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_c8(msg, np) result (res)
			complex(real64),intent(in) :: msg( :,:,:,:,:,: )
			complex(real64),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_c8(msg, np) result (res)
			complex(real64),intent(in) :: msg( :,:,:,:,:,:,: )
			complex(real64),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_c16(msg, np) result (res)
			complex(real128),intent(in) :: msg( : )
			complex(real128),allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_c16(msg, np) result (res)
			complex(real128),intent(in) :: msg( :,: )
			complex(real128),allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_c16(msg, np) result (res)
			complex(real128),intent(in) :: msg( :,:,: )
			complex(real128),allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_c16(msg, np) result (res)
			complex(real128),intent(in) :: msg( :,:,:,: )
			complex(real128),allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_c16(msg, np) result (res)
			complex(real128),intent(in) :: msg( :,:,:,:,: )
			complex(real128),allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_c16(msg, np) result (res)
			complex(real128),intent(in) :: msg( :,:,:,:,:,: )
			complex(real128),allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_c16(msg, np) result (res)
			complex(real128),intent(in) :: msg( :,:,:,:,:,:,: )
			complex(real128),allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d1_l(msg, np) result (res)
			logical,intent(in) :: msg( : )
			logical,allocatable :: res( : )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d2_l(msg, np) result (res)
			logical,intent(in) :: msg( :,: )
			logical,allocatable :: res( :,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d3_l(msg, np) result (res)
			logical,intent(in) :: msg( :,:,: )
			logical,allocatable :: res( :,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d4_l(msg, np) result (res)
			logical,intent(in) :: msg( :,:,:,: )
			logical,allocatable :: res( :,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d5_l(msg, np) result (res)
			logical,intent(in) :: msg( :,:,:,:,: )
			logical,allocatable :: res( :,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d6_l(msg, np) result (res)
			logical,intent(in) :: msg( :,:,:,:,:,: )
			logical,allocatable :: res( :,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_d7_l(msg, np) result (res)
			logical,intent(in) :: msg( :,:,:,:,:,:,: )
			logical,allocatable :: res( :,:,:,:,:,:,: )
			integer(int32),intent(in) :: np
		end function
		module function broadcast_si1(msg, np) result (res)
			integer(int8),intent(in) :: msg
			integer(int8) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_si2(msg, np) result (res)
			integer(int16),intent(in) :: msg
			integer(int16) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_si4(msg, np) result (res)
			integer(int32),intent(in) :: msg
			integer(int32) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_si8(msg, np) result (res)
			integer(int64),intent(in) :: msg
			integer(int64) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_sr4(msg, np) result (res)
			real(real32),intent(in) :: msg
			real(real32) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_sr8(msg, np) result (res)
			real(real64),intent(in) :: msg
			real(real64) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_sr16(msg, np) result (res)
			real(real128),intent(in) :: msg
			real(real128) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_sc4(msg, np) result (res)
			complex(real32),intent(in) :: msg
			complex(real32) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_sc8(msg, np) result (res)
			complex(real64),intent(in) :: msg
			complex(real64) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_sc16(msg, np) result (res)
			complex(real128),intent(in) :: msg
			complex(real128) :: res
			integer(int32),intent(in) :: np
		end function
		module function broadcast_sl(msg, np) result (res)
			logical,intent(in) :: msg
			logical :: res
			integer(int32),intent(in) :: np
		end function
		module function send_d1_i1(send_msg, pfrom, pto) result (recv_msg)
			integer(int8),intent(in) :: send_msg(:)
			integer(int8),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_i1(send_msg, pfrom, pto) result (recv_msg)
			integer(int8),intent(in) :: send_msg(:,:)
			integer(int8),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_i1(send_msg, pfrom, pto) result (recv_msg)
			integer(int8),intent(in) :: send_msg(:,:,:)
			integer(int8),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_i1(send_msg, pfrom, pto) result (recv_msg)
			integer(int8),intent(in) :: send_msg(:,:,:,:)
			integer(int8),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_i1(send_msg, pfrom, pto) result (recv_msg)
			integer(int8),intent(in) :: send_msg(:,:,:,:,:)
			integer(int8),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_i1(send_msg, pfrom, pto) result (recv_msg)
			integer(int8),intent(in) :: send_msg(:,:,:,:,:,:)
			integer(int8),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_i1(send_msg, pfrom, pto) result (recv_msg)
			integer(int8),intent(in) :: send_msg(:,:,:,:,:,:,:)
			integer(int8),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_i2(send_msg, pfrom, pto) result (recv_msg)
			integer(int16),intent(in) :: send_msg(:)
			integer(int16),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_i2(send_msg, pfrom, pto) result (recv_msg)
			integer(int16),intent(in) :: send_msg(:,:)
			integer(int16),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_i2(send_msg, pfrom, pto) result (recv_msg)
			integer(int16),intent(in) :: send_msg(:,:,:)
			integer(int16),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_i2(send_msg, pfrom, pto) result (recv_msg)
			integer(int16),intent(in) :: send_msg(:,:,:,:)
			integer(int16),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_i2(send_msg, pfrom, pto) result (recv_msg)
			integer(int16),intent(in) :: send_msg(:,:,:,:,:)
			integer(int16),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_i2(send_msg, pfrom, pto) result (recv_msg)
			integer(int16),intent(in) :: send_msg(:,:,:,:,:,:)
			integer(int16),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_i2(send_msg, pfrom, pto) result (recv_msg)
			integer(int16),intent(in) :: send_msg(:,:,:,:,:,:,:)
			integer(int16),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_i4(send_msg, pfrom, pto) result (recv_msg)
			integer(int32),intent(in) :: send_msg(:)
			integer(int32),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_i4(send_msg, pfrom, pto) result (recv_msg)
			integer(int32),intent(in) :: send_msg(:,:)
			integer(int32),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_i4(send_msg, pfrom, pto) result (recv_msg)
			integer(int32),intent(in) :: send_msg(:,:,:)
			integer(int32),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_i4(send_msg, pfrom, pto) result (recv_msg)
			integer(int32),intent(in) :: send_msg(:,:,:,:)
			integer(int32),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_i4(send_msg, pfrom, pto) result (recv_msg)
			integer(int32),intent(in) :: send_msg(:,:,:,:,:)
			integer(int32),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_i4(send_msg, pfrom, pto) result (recv_msg)
			integer(int32),intent(in) :: send_msg(:,:,:,:,:,:)
			integer(int32),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_i4(send_msg, pfrom, pto) result (recv_msg)
			integer(int32),intent(in) :: send_msg(:,:,:,:,:,:,:)
			integer(int32),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_i8(send_msg, pfrom, pto) result (recv_msg)
			integer(int64),intent(in) :: send_msg(:)
			integer(int64),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_i8(send_msg, pfrom, pto) result (recv_msg)
			integer(int64),intent(in) :: send_msg(:,:)
			integer(int64),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_i8(send_msg, pfrom, pto) result (recv_msg)
			integer(int64),intent(in) :: send_msg(:,:,:)
			integer(int64),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_i8(send_msg, pfrom, pto) result (recv_msg)
			integer(int64),intent(in) :: send_msg(:,:,:,:)
			integer(int64),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_i8(send_msg, pfrom, pto) result (recv_msg)
			integer(int64),intent(in) :: send_msg(:,:,:,:,:)
			integer(int64),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_i8(send_msg, pfrom, pto) result (recv_msg)
			integer(int64),intent(in) :: send_msg(:,:,:,:,:,:)
			integer(int64),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_i8(send_msg, pfrom, pto) result (recv_msg)
			integer(int64),intent(in) :: send_msg(:,:,:,:,:,:,:)
			integer(int64),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_r4(send_msg, pfrom, pto) result (recv_msg)
			real(real32),intent(in) :: send_msg(:)
			real(real32),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_r4(send_msg, pfrom, pto) result (recv_msg)
			real(real32),intent(in) :: send_msg(:,:)
			real(real32),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_r4(send_msg, pfrom, pto) result (recv_msg)
			real(real32),intent(in) :: send_msg(:,:,:)
			real(real32),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_r4(send_msg, pfrom, pto) result (recv_msg)
			real(real32),intent(in) :: send_msg(:,:,:,:)
			real(real32),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_r4(send_msg, pfrom, pto) result (recv_msg)
			real(real32),intent(in) :: send_msg(:,:,:,:,:)
			real(real32),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_r4(send_msg, pfrom, pto) result (recv_msg)
			real(real32),intent(in) :: send_msg(:,:,:,:,:,:)
			real(real32),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_r4(send_msg, pfrom, pto) result (recv_msg)
			real(real32),intent(in) :: send_msg(:,:,:,:,:,:,:)
			real(real32),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_r8(send_msg, pfrom, pto) result (recv_msg)
			real(real64),intent(in) :: send_msg(:)
			real(real64),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_r8(send_msg, pfrom, pto) result (recv_msg)
			real(real64),intent(in) :: send_msg(:,:)
			real(real64),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_r8(send_msg, pfrom, pto) result (recv_msg)
			real(real64),intent(in) :: send_msg(:,:,:)
			real(real64),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_r8(send_msg, pfrom, pto) result (recv_msg)
			real(real64),intent(in) :: send_msg(:,:,:,:)
			real(real64),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_r8(send_msg, pfrom, pto) result (recv_msg)
			real(real64),intent(in) :: send_msg(:,:,:,:,:)
			real(real64),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_r8(send_msg, pfrom, pto) result (recv_msg)
			real(real64),intent(in) :: send_msg(:,:,:,:,:,:)
			real(real64),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_r8(send_msg, pfrom, pto) result (recv_msg)
			real(real64),intent(in) :: send_msg(:,:,:,:,:,:,:)
			real(real64),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_r16(send_msg, pfrom, pto) result (recv_msg)
			real(real128),intent(in) :: send_msg(:)
			real(real128),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_r16(send_msg, pfrom, pto) result (recv_msg)
			real(real128),intent(in) :: send_msg(:,:)
			real(real128),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_r16(send_msg, pfrom, pto) result (recv_msg)
			real(real128),intent(in) :: send_msg(:,:,:)
			real(real128),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_r16(send_msg, pfrom, pto) result (recv_msg)
			real(real128),intent(in) :: send_msg(:,:,:,:)
			real(real128),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_r16(send_msg, pfrom, pto) result (recv_msg)
			real(real128),intent(in) :: send_msg(:,:,:,:,:)
			real(real128),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_r16(send_msg, pfrom, pto) result (recv_msg)
			real(real128),intent(in) :: send_msg(:,:,:,:,:,:)
			real(real128),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_r16(send_msg, pfrom, pto) result (recv_msg)
			real(real128),intent(in) :: send_msg(:,:,:,:,:,:,:)
			real(real128),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_c4(send_msg, pfrom, pto) result (recv_msg)
			complex(real32),intent(in) :: send_msg(:)
			complex(real32),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_c4(send_msg, pfrom, pto) result (recv_msg)
			complex(real32),intent(in) :: send_msg(:,:)
			complex(real32),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_c4(send_msg, pfrom, pto) result (recv_msg)
			complex(real32),intent(in) :: send_msg(:,:,:)
			complex(real32),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_c4(send_msg, pfrom, pto) result (recv_msg)
			complex(real32),intent(in) :: send_msg(:,:,:,:)
			complex(real32),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_c4(send_msg, pfrom, pto) result (recv_msg)
			complex(real32),intent(in) :: send_msg(:,:,:,:,:)
			complex(real32),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_c4(send_msg, pfrom, pto) result (recv_msg)
			complex(real32),intent(in) :: send_msg(:,:,:,:,:,:)
			complex(real32),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_c4(send_msg, pfrom, pto) result (recv_msg)
			complex(real32),intent(in) :: send_msg(:,:,:,:,:,:,:)
			complex(real32),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_c8(send_msg, pfrom, pto) result (recv_msg)
			complex(real64),intent(in) :: send_msg(:)
			complex(real64),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_c8(send_msg, pfrom, pto) result (recv_msg)
			complex(real64),intent(in) :: send_msg(:,:)
			complex(real64),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_c8(send_msg, pfrom, pto) result (recv_msg)
			complex(real64),intent(in) :: send_msg(:,:,:)
			complex(real64),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_c8(send_msg, pfrom, pto) result (recv_msg)
			complex(real64),intent(in) :: send_msg(:,:,:,:)
			complex(real64),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_c8(send_msg, pfrom, pto) result (recv_msg)
			complex(real64),intent(in) :: send_msg(:,:,:,:,:)
			complex(real64),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_c8(send_msg, pfrom, pto) result (recv_msg)
			complex(real64),intent(in) :: send_msg(:,:,:,:,:,:)
			complex(real64),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_c8(send_msg, pfrom, pto) result (recv_msg)
			complex(real64),intent(in) :: send_msg(:,:,:,:,:,:,:)
			complex(real64),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_c16(send_msg, pfrom, pto) result (recv_msg)
			complex(real128),intent(in) :: send_msg(:)
			complex(real128),allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_c16(send_msg, pfrom, pto) result (recv_msg)
			complex(real128),intent(in) :: send_msg(:,:)
			complex(real128),allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_c16(send_msg, pfrom, pto) result (recv_msg)
			complex(real128),intent(in) :: send_msg(:,:,:)
			complex(real128),allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_c16(send_msg, pfrom, pto) result (recv_msg)
			complex(real128),intent(in) :: send_msg(:,:,:,:)
			complex(real128),allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_c16(send_msg, pfrom, pto) result (recv_msg)
			complex(real128),intent(in) :: send_msg(:,:,:,:,:)
			complex(real128),allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_c16(send_msg, pfrom, pto) result (recv_msg)
			complex(real128),intent(in) :: send_msg(:,:,:,:,:,:)
			complex(real128),allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_c16(send_msg, pfrom, pto) result (recv_msg)
			complex(real128),intent(in) :: send_msg(:,:,:,:,:,:,:)
			complex(real128),allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d1_l(send_msg, pfrom, pto) result (recv_msg)
			logical,intent(in) :: send_msg(:)
			logical,allocatable :: recv_msg(:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d2_l(send_msg, pfrom, pto) result (recv_msg)
			logical,intent(in) :: send_msg(:,:)
			logical,allocatable :: recv_msg(:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d3_l(send_msg, pfrom, pto) result (recv_msg)
			logical,intent(in) :: send_msg(:,:,:)
			logical,allocatable :: recv_msg(:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d4_l(send_msg, pfrom, pto) result (recv_msg)
			logical,intent(in) :: send_msg(:,:,:,:)
			logical,allocatable :: recv_msg(:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d5_l(send_msg, pfrom, pto) result (recv_msg)
			logical,intent(in) :: send_msg(:,:,:,:,:)
			logical,allocatable :: recv_msg(:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d6_l(send_msg, pfrom, pto) result (recv_msg)
			logical,intent(in) :: send_msg(:,:,:,:,:,:)
			logical,allocatable :: recv_msg(:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_d7_l(send_msg, pfrom, pto) result (recv_msg)
			logical,intent(in) :: send_msg(:,:,:,:,:,:,:)
			logical,allocatable :: recv_msg(:,:,:,:,:,:,:)
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_si1(send_msg, pfrom, pto) result (recv_msg)
			integer(int8),intent(in) :: send_msg
			integer(int8) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_si2(send_msg, pfrom, pto) result (recv_msg)
			integer(int16),intent(in) :: send_msg
			integer(int16) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_si4(send_msg, pfrom, pto) result (recv_msg)
			integer(int32),intent(in) :: send_msg
			integer(int32) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_si8(send_msg, pfrom, pto) result (recv_msg)
			integer(int64),intent(in) :: send_msg
			integer(int64) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_sr4(send_msg, pfrom, pto) result (recv_msg)
			real(real32),intent(in) :: send_msg
			real(real32) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_sr8(send_msg, pfrom, pto) result (recv_msg)
			real(real64),intent(in) :: send_msg
			real(real64) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_sr16(send_msg, pfrom, pto) result (recv_msg)
			real(real128),intent(in) :: send_msg
			real(real128) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_sc4(send_msg, pfrom, pto) result (recv_msg)
			complex(real32),intent(in) :: send_msg
			complex(real32) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_sc8(send_msg, pfrom, pto) result (recv_msg)
			complex(real64),intent(in) :: send_msg
			complex(real64) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_sc16(send_msg, pfrom, pto) result (recv_msg)
			complex(real128),intent(in) :: send_msg
			complex(real128) :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function
		module function send_sl(send_msg, pfrom, pto) result (recv_msg)
			logical,intent(in) :: send_msg
			logical :: recv_msg
			integer(int32),intent(in) :: pfrom, pto
		end function

    end interface
    

    interface reduce_mpi
        procedure :: reduce_si1, reduce_si2, reduce_si4, reduce_si8, &
                     reduce_sr4, reduce_sr8, reduce_sr16, reduce_sc4, &
                     reduce_sc8, reduce_sc16, reduce_sl, reduce_d1_i1, &
                     reduce_d2_i1, reduce_d3_i1, reduce_d4_i1, reduce_d5_i1, &
                     reduce_d6_i1, reduce_d7_i1, reduce_d1_i2, reduce_d2_i2, &
                     reduce_d3_i2, reduce_d4_i2, reduce_d5_i2, reduce_d6_i2, &
                     reduce_d7_i2, reduce_d1_i4, reduce_d2_i4, reduce_d3_i4, &
                     reduce_d4_i4, reduce_d5_i4, reduce_d6_i4, reduce_d7_i4, &
                     reduce_d1_i8, reduce_d2_i8, reduce_d3_i8, reduce_d4_i8, &
                     reduce_d5_i8, reduce_d6_i8, reduce_d7_i8, reduce_d1_r4, &
                     reduce_d2_r4, reduce_d3_r4, reduce_d4_r4, reduce_d5_r4, &
                     reduce_d6_r4, reduce_d7_r4, reduce_d1_r8, reduce_d2_r8, &
                     reduce_d3_r8, reduce_d4_r8, reduce_d5_r8, reduce_d6_r8, &
                     reduce_d7_r8, reduce_d1_r16, reduce_d2_r16, reduce_d3_r16, &
                     reduce_d4_r16, reduce_d5_r16, reduce_d6_r16, reduce_d7_r16, &
                     reduce_d1_c4, reduce_d2_c4, reduce_d3_c4, reduce_d4_c4, &
                     reduce_d5_c4, reduce_d6_c4, reduce_d7_c4, reduce_d1_c8, &
                     reduce_d2_c8, reduce_d3_c8, reduce_d4_c8, reduce_d5_c8, &
                     reduce_d6_c8, reduce_d7_c8, reduce_d1_c16, reduce_d2_c16, &
                     reduce_d3_c16, reduce_d4_c16, reduce_d5_c16, reduce_d6_c16, &
                     reduce_d7_c16, reduce_d1_l, reduce_d2_l, reduce_d3_l, &
                     reduce_d4_l, reduce_d5_l, reduce_d6_l, reduce_d7_l
    end interface

    public :: reduce_mpi

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

    interface bcast_mpi
        procedure :: broadcast_d1_i1, broadcast_d2_i1, broadcast_d3_i1, broadcast_d4_i1, &
                     broadcast_d5_i1, broadcast_d6_i1, broadcast_d7_i1, broadcast_d1_i2, &
                     broadcast_d2_i2, broadcast_d3_i2, broadcast_d4_i2, broadcast_d5_i2, &
                     broadcast_d6_i2, broadcast_d7_i2, broadcast_d1_i4, broadcast_d2_i4, &
                     broadcast_d3_i4, broadcast_d4_i4, broadcast_d5_i4, broadcast_d6_i4, &
                     broadcast_d7_i4, broadcast_d1_i8, broadcast_d2_i8, broadcast_d3_i8, &
                     broadcast_d4_i8, broadcast_d5_i8, broadcast_d6_i8, broadcast_d7_i8, &
                     broadcast_d1_r4, broadcast_d2_r4, broadcast_d3_r4, broadcast_d4_r4, &
                     broadcast_d5_r4, broadcast_d6_r4, broadcast_d7_r4, broadcast_d1_r8, &
                     broadcast_d2_r8, broadcast_d3_r8, broadcast_d4_r8, broadcast_d5_r8, &
                     broadcast_d6_r8, broadcast_d7_r8, broadcast_d1_r16, broadcast_d2_r16, &
                     broadcast_d3_r16, broadcast_d4_r16, broadcast_d5_r16, broadcast_d6_r16, &
                     broadcast_d7_r16, broadcast_d1_c4, broadcast_d2_c4, broadcast_d3_c4, &
                     broadcast_d4_c4, broadcast_d5_c4, broadcast_d6_c4, broadcast_d7_c4, &
                     broadcast_d1_c8, broadcast_d2_c8, broadcast_d3_c8, broadcast_d4_c8, &
                     broadcast_d5_c8, broadcast_d6_c8, broadcast_d7_c8, broadcast_d1_c16, &
                     broadcast_d2_c16, broadcast_d3_c16, broadcast_d4_c16, broadcast_d5_c16, &
                     broadcast_d6_c16, broadcast_d7_c16, broadcast_d1_l, broadcast_d2_l, &
                     broadcast_d3_l, broadcast_d4_l, broadcast_d5_l, broadcast_d6_l, &
                     broadcast_d7_l, broadcast_si1, broadcast_si2, broadcast_si4, &
                     broadcast_si8, broadcast_sr4, broadcast_sr8, broadcast_sr16, &
                     broadcast_sc4, broadcast_sc8, broadcast_sc16, broadcast_sl
    end interface

    public :: bcast_mpi

    interface send_mpi
        procedure :: send_si1, send_si2, send_si4, send_si8, &
                     send_sr4, send_sr8, send_sr16, send_sc4, &
                     send_sc8, send_sc16, send_sl, send_d1_i1, &
                     send_d2_i1, send_d3_i1, send_d4_i1, send_d5_i1, &
                     send_d6_i1, send_d7_i1, send_d1_i2, send_d2_i2, &
                     send_d3_i2, send_d4_i2, send_d5_i2, send_d6_i2, &
                     send_d7_i2, send_d1_i4, send_d2_i4, send_d3_i4, &
                     send_d4_i4, send_d5_i4, send_d6_i4, send_d7_i4, &
                     send_d1_i8, send_d2_i8, send_d3_i8, send_d4_i8, &
                     send_d5_i8, send_d6_i8, send_d7_i8, send_d1_r4, &
                     send_d2_r4, send_d3_r4, send_d4_r4, send_d5_r4, &
                     send_d6_r4, send_d7_r4, send_d1_r8, send_d2_r8, &
                     send_d3_r8, send_d4_r8, send_d5_r8, send_d6_r8, &
                     send_d7_r8, send_d1_r16, send_d2_r16, send_d3_r16, &
                     send_d4_r16, send_d5_r16, send_d6_r16, send_d7_r16, &
                     send_d1_c4, send_d2_c4, send_d3_c4, send_d4_c4, &
                     send_d5_c4, send_d6_c4, send_d7_c4, send_d1_c8, &
                     send_d2_c8, send_d3_c8, send_d4_c8, send_d5_c8, &
                     send_d6_c8, send_d7_c8, send_d1_c16, send_d2_c16, &
                     send_d3_c16, send_d4_c16, send_d5_c16, send_d6_c16, &
                     send_d7_c16, send_d1_l, send_d2_l, send_d3_l, &
                     send_d4_l, send_d5_l, send_d6_l, send_d7_l
    end interface

    public :: send_mpi

    
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
