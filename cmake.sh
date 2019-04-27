#!bin/bash
rm -r build 2>/dev/null
mkdir build 2>/dev/null
cd build
cmake ..
make
