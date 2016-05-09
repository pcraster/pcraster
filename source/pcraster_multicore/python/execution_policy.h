#pragma once

#include "fern/algorithm/policy/execution_policy.h"


namespace pcraster_multicore {
namespace python {

void               set_nr_cpus         (size_t cpus);

size_t             nr_cpus             ();

fern::algorithm::ExecutionPolicy const&
                   execution_policy    ();

} // namespace python
} // namespace pcraster_multicore
