/* this file is created by oplist. do not edit */
/* EXEC_DASS jump table node */
/* fluxes and (spread)zones are always second */
{{OP_ACCUSTATE,OP_ACCUFLUX},static_cast<DO_DASS>(Do_accu)},
{{OP_ACCUCAPACITYSTATE,OP_ACCUCAPACITYFLUX},static_cast<DO_DASS>(Do_accucapacity)},
{{OP_ACCUTHRESHOLDSTATE,OP_ACCUTHRESHOLDFLUX},static_cast<DO_DASS>(Do_accuthreshold)},
{{OP_ACCUFRACTIONSTATE,OP_ACCUFRACTIONFLUX},static_cast<DO_DASS>(Do_accufraction)},
{{OP_ACCUTRIGGERSTATE,OP_ACCUTRIGGERFLUX},static_cast<DO_DASS>(Do_accutrigger)},
{{OP_ACCUTRAVELTIMESTATE,OP_ACCUTRAVELTIMEFLUX},static_cast<DO_DASS>(Do_accutraveltime)},
{{OP_DIFFUSESTATE,OP_DIFFUSEFLUX},static_cast<DO_DASS>(Do_diffuse)},
{{OP_SPREAD,OP_SPREADZONE},static_cast<DO_DASS>(Do_spread_impl)},
{{OP_SPREADMAX,OP_SPREADMAXZONE},static_cast<DO_DASS>(Do_spreadmax_impl)},
{{OP_SPREADLDD,OP_SPREADLDDZONE},static_cast<DO_DASS>(Do_spreadldd_impl)},
{{OP_LDDCREATE,OP_LDDCREATEDEM},static_cast<DO_DASS>(Do_lddcreate_impl)},
{{OP_DYNAMICWAVEQ,OP_DYNAMICWAVEH},static_cast<DO_DASS>(Do_dynamicwave)},
