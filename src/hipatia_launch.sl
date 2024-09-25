#!/bin/bash
#SBATCH -J Lub_rheo
#SBATCH --mem-per-cpu=1G  # memory/cpu
#SBATCH -p large # Partitio

srun biviscous_suspension
