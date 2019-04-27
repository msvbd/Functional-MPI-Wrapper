#
#
# This script compile and run all examples in /examples
# by mpifort and mpirun on 4 processor
#
#

# Glob: https://docs.python.org/2/library/glob.html
import glob

# OS: https://docs.python.org/2/library/os.html
import os

for ftest in glob.glob('examples/*.f90'):
	print("compile "+ftest)
	os.system('mpifort '+ftest+' -J./build/include -L./build/lib -lmpifw')
	print("run "+ftest)
	os.system('mpirun -n 4 a.out')
	print("\n")
	
os.system('rm a.out')
