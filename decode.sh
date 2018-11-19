#!/bin/bash -f
###### a script to decode NCEP CFSv2 operational products automatically ######
###### written by Li Zhenkun, Jul. 13, 2016 ##################################

###### set some environment varibles ################################################
export CP_INTEL_DIR=/opt/software/compiler/intel/composer_xe_2015.2.164
export LD_LIBRARY_PATH=${CP_INTEL_DIR}/compiler/lib/intel64:${CP_INTEL_DIR}/mkl/lib/intel64
export LIBRARY_PATH=${CP_INTEL_DIR}/compiler/lib/intel64:${CP_INTEL_DIR}/mkl/lib/intel64
#export GRIB_API_LIB=/opt/software/libs/grib_api/1.13.0/intel/lib
#export GRIB_API_INCLUDE=/opt/software/libs/grib_api/1.13.0/intel/include
#export LD_LIBRARY_PATH=${GRIB_API_LIB}:${LD_LIBRARY_PATH}
export ECCODE_LIB=/home/lizhenkun/eccodes/intel/lib
export ECCODE_INCLUDE=/home/lizhenkun/eccodes/intel/include
export LD_LIBRARY_PATH=${ECCODE_LIB}:${LD_LIBRARY_PATH}
#####################################################################################

###### set some useful parameters for convenience ######
dt=3
date=`date -d " $dt days ago " +%Y%m%d`
input_dir=/data/cfs
output_dir=/home/lizhenkun/cfs/cfs_operation_output
decode_exe_dir=/home/lizhenkun/cfs/cfs_decode
decode_exe=cfs_decode.exe
########################################################

echo '=====> Begin to decode NCEP CFSv2 operational products <====='
cd $decode_exe_dir
for var_name in z500 z200 wnd850 prate tmp2m
do
  if [ ! -e $output_dir/$var_name/$var_name-$date-cfs-forecast.dat ]; then
    if [ -e $output_dir/namelist ]; then
      rm -f $output_dir/namelist
    fi
    cat > $output_dir/namelist << EOF
&params
start_date    = $date
end_date      = $date
var_name      = '$var_name'
input_dir     = '$input_dir'
output_dir    = '$output_dir/$var_name'
/
EOF
    echo '  decoding '$var_name' now ...'
    ./$decode_exe $output_dir/namelist
  fi
done

echo ' all jobs successfully finished'
