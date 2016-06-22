#!/usr/bin/env bash
hostname=`hostname`

if [[ $hostname == "sonic.geo.uu.nl" ]]; then
    max_nr_threads=20

elif [[ $hostname == "gransasso" ]]; then
    max_nr_threads=6
if


measure_performance.py \
    --max-nr-threads=$max_nr_threads \
    $PCRASTER_PERFORMANCE_DATA
create_plots.py \
    --max-nr-threads=$max_nr_threads
