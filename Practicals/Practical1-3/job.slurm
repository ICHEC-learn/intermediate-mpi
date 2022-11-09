#!/bin/bash

#SBATCH --partition=CourseDevQ
#SBATCH --reservation=CourseNov9
#SBATCH --nodes=1
#SBATCH --time=00:05:00
#SBATCH --account=course
#SBATCH --output=pingpong.log

cd $SLURM_SUBMIT_DIR

#Load module
module load openmpi

mpirun -np 2 ./pingpong
