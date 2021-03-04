#!/bin/bash

rm NeMo*.o*
rm NeMo*.e*

#rm -rf ./derivatives/NeMo_output
rm -rf $WORK/local_cluster_jobs


export MATLAB_PREFDIR=$WORK/.matlab


CHUNKS=8

cnt=0
for visit in V3; do
	for atlassize in 86; do
		job=${visit}_${atlassize}
		sbatch --job-name=$job --error ${job}.e%j --output=${job}.o%j \
			--ntasks=32 --time=11:55:00 --partition=std --array=0-$((CHUNKS-1)) \
			--export=MATLAB_PREFDIR \
			parcompute.sh ${visit} ${atlassize} ascend
		pids[${cnt}]=$!
		cnt=$[cnt+1]
	done
done
