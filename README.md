## Functional MPI Wrapper

Functional MPI wrapper for Fortran. 

* [Getting started](#getting-started)
* [Why MPI wrapper?](#why-mpi-wrapper)
* [What's included?](#whats-included)
* [Example usage](#example-usage)
* [References](#references)

## Getting started
You need some MPI Fortran wrapper compiler. For example: `mpifort`, `mpiifort`, etc.

Example usage:
```
mpifort -O3 -c mod_mpi_wrapper.f90 -o  mod_mpi_wrapper
mpifort my_awesome_parallel_code.f90 -o my_awesome_parallel_code mod_mpi_wrapper
mpirun -n 4 ./my_awesome_parallel_code
```

## Why MPI wrapper?

MPI wrapper contains overloaded functions for a common functions
of [MPI API](https://www.open-mpi.org/doc/current/man3/MPI.3.php).
The wrapper enables you to write your parallel code more semantic,
 more simple and more functional. Currently it includes just few common functions
 and overloaded functions,
see [What's included?](#whats-included). 

## What's included?

Variables:
* `num_procs` - number of MPI processes.
* `this_proc` - id of calling process. It can range from 0 to `num_procs`-1.

Subroutines:
* `init_mpi()` - Initializes the MPI execution environment and set _Variables_
	* Wrapper for [`MPI_INIT`](https://www.open-mpi.org/doc/current/man3/MPI_Init.3.php), 
			   [`MPI_COMM_SIZE`](https://www.open-mpi.org/doc/current/man3/MPI_Comm_size.3.php)
			    and [`MPI_COMM_RANK`](https://www.open-mpi.org/doc/current/man3/MPI_Comm_rank.3.php).
* `finish_mpi()` - Terminates MPI execution environment
	* Wrapper for [`MPI_FINALIZE`](https://www.open-mpi.org/doc/current/man3/MPI_Finalize.3.php).
* `sync_mpi()` - Synchronization of MPI processes
	* Wrapper for [`MPI_BARRIER`](https://www.open-mpi.org/doc/current/man3/MPI_Barrier.3.php).
* `stop_mpi([msg])` - Terminates MPI execution environment. Replacement for a `stop` statement.
	Argugement `msg` is optional character variable which will send to STDOUT befor tirmination.*
	* Wrapper for [`MPI_ABORT`](https://www.open-mpi.org/doc/current/man3/MPI_Abort.3.php).

Functions:
* `send_mpi(msg, p_from, p_to)` - Send `msg` from process `p_from` to process `p_to`
	* Wrapper for [`MPI_SEND`](https://www.open-mpi.org/doc/current/man3/MPI_Send.3.php)
					 and [`MPI_RECV`](https://www.open-mpi.org/doc/current/man3/MPI_Recv.3.php). 
* `bcast_mpi(msg, p_from)` - Send `msg` from `p_from` to all processes
	* Wrapper for [`MPI_BCAST`](https://www.open-mpi.org/doc/current/man3/MPI_Bcast.3.php). 
* `reduce_mpi(msg, n_to, op)` - Reduce `msg` on all process via operation
	`op` and result send to process `n_to`
	* Wrapper for [`MPI_REDUCE`](https://www.open-mpi.org/doc/current/man3/MPI_Reduce.3.php). 
* `allreduce_mpi(msg, op)` - Same as the `reduce_mpi` but the result is send to
	all processes
	* Wrapper for [`MPI_ALLREDUCE`](https://www.open-mpi.org/doc/current/man3/MPI_Allreduce.3.php). 


All of the above functions are compatible with the standard Fortran 2008 kinds:
`int8, `int16`, `int32`, `int64` `real32`, `real64`, `real128` 
`complex(real32)`, `complex(real64)`, `complex(real128)` and `logical`

Argumets:
* `msg` - can by scalar or array with dimension of range 1 to 7
		- accepted kinds: `int8, `int16`, `int32`, `int64` `real32`, `real64`, `real128` 
			`complex(real32)`, `complex(real64)`, `complex(real128)` and `logical`
* `p_from`, `p_to` - are `int32` variables with range from 0 to `num_procs`-1.
* `op` - is `character` and can take the vlues:
	* `"sum"` - sum
	* `"prod"` - product
	* `"min"` - minimum value
	* `"max"` - maximum value
	* `"and"` - logical and
	* `"or"` - logical or

## Example usage
Examples you can found in `/examples`
### `send_mpi` example
```
use mod_mpi_wrapper
use iso_fortran_env
implicit none

integer(int32) :: i, a(3) , b(3)    
    
call init_mpi()

a = [( this_proc*i , i=1,3)]
b = 0

write(*,'(*(g0,1x))') this_proc,")", a

call sync_mpi()
if(this_proc==0) write(*,*) "=================="
call sync_mpi()

b = send_mpi(a, 1, 3)

write(*,'(*(g0,1x))') this_proc,")",a,"|",b

call finish_mpi()
```
**Typical output (for 4 processes):**
```
1 ) 1 2 3
3 ) 3 6 9
2 ) 2 4 6
0 ) 0 0 0
 ==================
0 ) 0 0 0 | 0 0 0
3 ) 3 6 9 | 1 2 3
1 ) 1 2 3 | 1 2 3
2 ) 2 4 6 | 2 4 6
```
### `bcast_mpi` example
```
use mod_mpi_wrapper
use iso_fortran_env
implicit none

integer(int32) :: i, a(3) , b(3)    
    
call init_mpi()

a = [( this_proc*i , i=1,3)]

write(*,'(*(g0,1x))') this_proc,")", a

call sync_mpi()
if(this_proc==0) write(*,*) "=================="
call sync_mpi()

a = bcast_mpi(a, 3)

write(*,'(*(g0,1x))') this_proc,")", a

call finish_mpi()
```
**Typical output (for 4 processes):**
```
1 ) 1 2 3
2 ) 2 4 6
3 ) 3 6 9
0 ) 0 0 0
 ==================
3 ) 3 6 9
1 ) 3 6 9
2 ) 3 6 9
0 ) 3 6 9
```

### `reduce_mpi` and `allreduce_mpi` example
```
use mod_mpi_wrapper
use iso_fortran_env
implicit none

integer(int32) :: i, a(3) , b(3)  
    
call init_mpi()

a = [( this_proc*i , i=1,3)]

write(*,'(*(g0,1x))') this_proc,")", a

call sync_mpi()
if(this_proc==0) write(*,*) "=================="
call sync_mpi()

write(*,*) this_proc,reduce_mpi(a, 0, "sum")

call sync_mpi()
if(this_proc==0) write(*,*) "=================="
call sync_mpi()

write(*,'(*(g0,1x))') this_proc,")",allreduce_mpi(a, "sum")

call finish_mpi()
```
**Typical output (for 4 processes):**
```
0 ) 0 0 0
3 ) 3 6 9
1 ) 1 2 3
2 ) 2 4 6
 ==================
3 ) 3 6 9
1 ) 1 2 3
2 ) 2 4 6
0 ) 6 12 18
 ==================
2 ) 6 12 18
3 ) 6 12 18
1 ) 6 12 18
0 ) 6 12 18
```

## References

* [https://www.open-mpi.org/](https://www.open-mpi.org/)
	[Documentation](https://www.open-mpi.org/doc/current/)

* [http://www.mpich.org/](http://www.mpich.org/)
