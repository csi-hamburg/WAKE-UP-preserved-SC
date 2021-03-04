#!/bin/bash


source /sw/batch/init.sh

module load matlab


if [ -z ${SLURM_ARRAY_TASK_COUNT+x} ]; then
	SLURM_ARRAY_TASK_COUNT=1
fi

if [ -z ${SLURM_ARRAY_TASK_ID+x} ]; then
	SLURM_ARRAY_TASK_ID=0
fi

cd ./code/NeMo_analysis_matlab && matlab  -nodisplay -r "visit='$1'; atlassize=$2; procflag='compute'; CHUNKS=$SLURM_ARRAY_TASK_COUNT; MAGICNUMBER=$SLURM_ARRAY_TASK_ID; SORT_MODE='$3'; run; exit"
