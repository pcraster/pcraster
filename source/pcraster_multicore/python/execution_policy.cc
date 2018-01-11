#include <iostream>

#include "execution_policy.h"


namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {


static fa::ExecutionPolicy _execution_policy{
    fa::ParallelExecutionPolicy{}};


static size_t _nr_worker_threads{std::thread::hardware_concurrency()};


void set_nr_worker_threads(size_t threads){
  size_t max_threads = std::thread::hardware_concurrency();

  if(threads > max_threads){
    std::cout << "Amount of worker threads requested (" << threads << ") exceeds maximum number of threads; limiting to " << max_threads << " threads.\n";
    threads = max_threads;
  }

  if(threads < 1){
    _nr_worker_threads = 0;
    _execution_policy = fa::SequentialExecutionPolicy{};
  }
  else{
    _nr_worker_threads = threads;
    _execution_policy = fa::ParallelExecutionPolicy{threads};
  }
}


fa::ExecutionPolicy const& execution_policy(){
  return _execution_policy;
}


size_t nr_worker_threads(){
  return _nr_worker_threads;
}


} // namespace python
} // namespace pcraster_multicore
