#include <iostream>

#include "execution_policy.h"


namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {


static fa::ExecutionPolicy _execution_policy{
    fa::parallel};

static size_t _nr_cpus{1};

void set_nr_cpus(size_t cpus){
  size_t max_cpus = std::thread::hardware_concurrency();
  if(cpus > max_cpus){
    std::cout << "Number of CPUs requested (" << cpus << ") larger than CPUs available, limiting to " << max_cpus << " CPUs" << std::endl;
    cpus = max_cpus;
  }
  if(cpus < 1){
    cpus = 1;
  }
  _nr_cpus = cpus;
  fa::parallel = fa::ParallelExecutionPolicy{cpus};
  _execution_policy = fa::parallel;
}


fa::ExecutionPolicy const& execution_policy(){
  return _execution_policy;
}

size_t nr_cpus(){
  return _nr_cpus;
}

} // namespace python
} // namespace pcraster_multicore
