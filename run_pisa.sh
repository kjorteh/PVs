#!/bin/bash

COUNTRY=$1

cp -r /opt/cmdstan/cmdstan-2.38.0 .
export CMDSTAN_PATH="$(pwd)/cmdstan-2.38.0"

Rscript Sparsity_PISA_HTC.R $COUNTRY