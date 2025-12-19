#ifndef INCLUDED_CALC_DOMAINILL
#define INCLUDED_CALC_DOMAINILL

#include "stddefx.h"


namespace calc {

//! type of possible illegal domain
enum DomainIll {
    noDomainIll,    /*!< no illegal domain */
    onlyDomainIll,  /*!< unary argument has domain check */
    rightDomainIll, /*!< right (2nd) argument has domain check */
    combDomainIll   /*!< combination of 1st and 2nd arg may be illegal, pow only */
};

} // namespace calc

#endif
