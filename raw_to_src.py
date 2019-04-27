#
#
#
# This script create compiled f90 files in /src from "raw" f90 files in /raw
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
			
# @PROCEDURE_START@
# @PROCEDURE_END@

# @INTERFACE_START@
# @INTERFACE_END@

# @F_KIND_ID@
# @F_KIND@
# @MPI_KIND@
# @DIM_ID@
# @RANK@

# @INTERFACE_NAME@
# @OVERLOADED_INTERFACES@
# @INTERFACES@

	
def copy_block(block):
	multiblock = ""
	
	if re.search(r'@DIM_ID@',block):
		for i in range(len(f_kind)):
			for irank in range(1,8):
				multiblock += block.replace("@F_KIND@", f_kind[i] ).\
									replace("@F_KIND_ID@", f_kind_id[i] ).\
									replace("@MPI_KIND@", mpi_kind[i] ).\
									replace("@DIM_ID@", "d"+str(irank) ).\
									replace("@RANK@",  ":" if irank==1 else ":,"*(irank-1)+":")
									
	elif re.search(r'@F_KIND_ID@',block):
		for i in range(len(f_kind)):
			multiblock += block.replace("@F_KIND@", f_kind[i] ).\
								replace("@F_KIND_ID@", f_kind_id[i] ).\
								replace("@MPI_KIND@", mpi_kind[i] )
								
	else:
		multiblock += block
	
	return multiblock

try:
	os.mkdir("src")
except:
	pass
	
start_procedure = False
start_interface = False
overloaded_interface = ""
interfaces = ""

for rawf in glob.glob('./raw/raw_s*.f90'):
	
	content = ""
	interface_name=""
	for line in open(rawf, "rt"):
		if re.search('\s*\@INTERFACE_NAME\@',line):
			interface_name = re.findall(r"(?<=INTERFACE_NAME\@).+?(?=\@)", line)[0].strip()
			
		elif re.search('\s*\@INTERFACE_START\@',line):
			start_interface = not start_interface
			block = ""
		elif re.search('\s*\@INTERFACE_END\@',line):
			start_interface = not start_interface
			interfaces += copy_block(block)
			
		elif re.search('\s*\@PROCEDURE_START\@',line):
			start_procedure = not start_procedure
			block = ""
		elif re.search('\s*\@PROCEDURE_END\@',line):
			start_procedure = not start_procedure
			content += copy_block(block)
			
		else:
			if start_procedure or start_interface:
				block += line
			else:
				content += line
	
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
	
	if interface_name:
		overloaded_interface += "\n    interface "+interface_name+"\n"
		overloaded_interface += "        procedure :: "+string_list
		overloaded_interface += "\n    end interface\n"
		overloaded_interface += "\n    public :: "+interface_name+"\n"

	fout = open(rawf.replace("raw_","").replace("/raw/","/src/"), "wt")
	fout.write(content)
	fout.close()

fin = open("./raw/raw_mod_mpifw.f90", "rt")
fout = open("./src/mod_mpifw.f90", "wt")
content = fin.read()
fout.write(content.replace("@OVERLOADED_INTERFACES@", overloaded_interface )\
				  .replace("@INTERFACES@", interfaces))
fin.close()
fout.close()
