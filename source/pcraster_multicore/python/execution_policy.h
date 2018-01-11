#pragma once

#include "fern/algorithm/policy/execution_policy.h"


namespace pcraster_multicore {
namespace python {

void               construct_execution_policy();

void               destruct_execution_policy();

void               set_nr_worker_threads(size_t threads);

size_t             nr_worker_threads   ();

fern::algorithm::ExecutionPolicy const&
                   execution_policy    ();

} // namespace python
} // namespace pcraster_multicore
