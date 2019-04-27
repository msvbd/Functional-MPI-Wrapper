#
#
#
# This create compiled f90 files in /src from "raw" f90 files in /raw
#
#
#

# RegExp: https://docs.python.org/2/library/re.html
import re

# Glob: https://docs.python.org/2/library/glob.html
import glob

# OS: https://docs.python.org/2/library/os.html
import os


# int8, int16, int32, int64
# real32, real64, real128 
# complex(real32), complex(real64), complex(real128)

f_kind = ("integer(int8)", "integer(int16)", "integer(int32)", "integer(int64)", \
			"real(real32)", "real(real64)", "real(real128)", \
			"complex(real32)", "complex(real64)", "complex(real128)", \
			"logical" )
			
f_kind_id = ("i1", "i2", "i4", "i8", \
			"r4", "r8", "r16", \
			"c4", "c8", "c16", \
			"l")
			
mpi_kind = ("MPI_INTEGER1", "MPI_INTEGER2", "MPI_INTEGER4", "MPI_INTEGER8", \
			"MPI_REAL4", "MPI_REAL8", "MPI_REAL16", \
			"MPI_COMPLEX8", "MPI_COMPLEX16", "MPI_COMPLEX32", \
			"MPI_LOGICAL")
			
# @LIST@

# @SCALAR_START@
# @SCALAR_END@

# @ARRAY_START@
# @ARRAY_END@

# @F_KIND_ID@
# @F_KIND@
# @MPI_KIND@
# @DIM_ID@
# @RANK@

def copy_scalar_block(block):
	multiblock = ""
	for i in range(len(f_kind)):
		multiblock += block.replace("@F_KIND@", f_kind[i]).\
						    replace("@F_KIND_ID@", f_kind_id[i]).\
						    replace("@MPI_KIND@", mpi_kind[i])
	return multiblock
	
def copy_array_block(block):
	multiblock = ""
	for i in range(len(f_kind)):
		for irank in range(1,8):
			multiblock += block.replace("@F_KIND@", f_kind[i] ).\
								replace("@F_KIND_ID@", f_kind_id[i] ).\
								replace("@MPI_KIND@", mpi_kind[i] ).\
								replace("@DIM_ID@", "d"+str(irank) ).\
								replace("@RANK@",  ":" if irank==1 else ":,"*(irank-1)+":")
	return multiblock

try:
	os.mkdir("src")
except:
	pass
	
start_scalar = False
start_array = False

#for rawf in glob.glob('./raw/*'):
for rawf in glob.glob('./raw/*.f90'):
	
	content = ""
	for line in open(rawf, "rt"):
		if re.match('\@SCALAR_START\@',line):
			start_scalar = not start_scalar
			scalar_block = ""
		elif re.match('\@SCALAR_END\@',line):
			start_scalar = not start_scalar
			content += copy_scalar_block(scalar_block)
			#fout.write(copy_scalar_block(scalar_block))
			
		elif re.match('\@ARRAY_START\@',line):
			start_array = not start_array
			array_block = ""
		elif re.match('\@ARRAY_END\@',line):
			start_array = not start_array
			content += copy_array_block(array_block)
			#fout.write(copy_array_block(array_block))
		else:
			if start_scalar:
				scalar_block += line
			elif start_array:
				array_block += line
			else:
				content += line
				#fout.write(line)
	
	procedure_list = re.findall(r"(?<=function\s).+?(?=\()", content)
	procedure_list.extend(re.findall(r"(?<=subroutine\s).+?(?=\()", content))
	string_list = ""
	for i in range(len(procedure_list)-1):
		string_list += procedure_list[i]+", "
		if (i+1)%4==0: string_list += "&\n                     "
	try:
		string_list += procedure_list[len(procedure_list)-1]
	except:
		pass
	
	#procedure_list = ""
	fout = open(rawf.replace("raw_","").replace("/raw/","/src/"), "wt")
	fout.write(content.replace("@LIST@", string_list))
	fout.close()
