# Measuring PCRaster's performance
Rules of thumb:
- Only measure performance of optimized **release** builds
- Only store and keep valid measurements in the database
- Run the performance measurements while the machine is available
- Make sure data used by the measurements fits in memory
    - Unload data ASAP
    - Some algorithms allocate temporary memory!
- Don't test for correctness of the results


## Introduction
This directory stores scripts for measuring the performance of PCRaster. The goals of these measurements are:

- How does the performance of each individual operation vary over time? A decrease in performance is a bug and we need to know if and when it happens. There are various reasons why the performance of an operation might change over time:
    - Change in the implementation
    - Change in compiler or compiler options
    - Change in 3rd library version PCRaster depends on
    - ...
- How does the performance of a new implementation of an operation compare against the original version? When developing alternative implementations of operations it is necessary to know how the performance is impacted (see also first bullet).
- In the case of scalable algorithms, how does the performance scale over the hardware resources?


The requirements for the measurement scripts and process are:
- Performance measurements must be stored in a database. Each new measurement must result in a new, timestamped record in the database.
- Measurements on different platforms (OS and hardware) must be stored in different databases.
- The results of the performance measurements must be easy to interpret. They must be visualized as graphs and tables.
- There must be a single script which does everything needed to perform all performance measurements. By default, it must store the results in a platform-specific database, named after the OS and archicture.
- The platform-specific database must be stored in a safe place. This is not a file we want to risk losing. Its historic contents cannot be reproduced.
- For each performance measurement, it must be known what version of PCRaster was used, including the Git commit-id.


## Requirements
The code requires the folowing packages:
- [Performance analyst](https://github.com/pcraster/performance_analyst) (≥ 0.0.13)
- GDAL 2.x Python package
- Numpy Python package
- Matplotlib Python package
- [psutil](https://github.com/giampaolo/psutil)



## Input data
Performance measurements require input data. These can be large datasets. We need a script that generates data when necessary. The performance measurements can then load this data. This prevents the generation of data sets each time the measurements are run.

```bash
create_datasets.py /data/pcraster/performance
```


## Performing measurements
To perform measurements, just run the `measure_performance.py` script:

```bash
measure_performance.py /data/pcraster/performance
```

By default, this script will run all performance tests and store them in a platform-specific database. See `measure_performance.py --help` for help about this command, especially about overriding the name and location of the platform-specific database.

The database is an sqlite database which can be queries using SQL. The `pa.py` script supports manipulating the database. See its help for more info.


## Visualizing results
To visualize the performance measurements results you can use the `create_plots.py` script:

```bash
create_plots.py
```

This command will read the platform specific database with the results and create a number of pdfs:
- For each operation a pdf with evolution of performance over time. These pdfs are names after the timer cases.
- For each set of alternative operations a pdf with evolution of performance over time. These pdfs are named after the operations.


## Results

### Classic versus multicore
Comparison of performance of classic versus multicore algorithm, using a single thread (the main thread: sequential execution).

The performance of the new algorithms must be as good as the current ones.

Multicore algorithms that must be improved:
- (add)
- ifthen
- (multiply)
- (power)
- (slope)
- (subtract)


### Scalability
Comparison of performance of multicore algorithms using a different number of worker threads.

The performance must scale with the number of threads. Using twice the number of threads should half the amount of time needed to calculate the result.

Multicore algorithms that must be improved:

- max
- min
- window4total


## Links
- [Performance analyst](https://github.com/pcraster/performance_analyst)
