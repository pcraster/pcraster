#ifndef __STDIO
#include <stdio.h>
#define __STDIO
#endif

// #undef NDEBUG

#ifdef DEBUG_DEVELOP
#include <assert.h>
# define DEVELOP_POSTCOND(cond)   assert(cond)
# define DEVELOP_PRECOND(cond)    assert(cond)
#else
# define DEVELOP_POSTCOND(cond)
# define DEVELOP_PRECOND(cond)
#endif

#ifdef DEBUG

/* avoid statement has no effect in
 * a constant precondition 
 * a source file that use PRECOND_CONST
 * must insert USE_PRECOND_CONST above
 * the USE header
 */
#define USE_PRECOND_CONST static int constCond_t_=1;


# include <assert.h>

/* deprecated  use ifdef instead */
# define IFDEBUG(action)      action

# define POSTCOND(cond)       assert(cond)
# define PRECOND(cond)        assert(cond)
# define PRECOND_CONST(cond)  assert(constCond_t_ && (cond))

#else /* DEBUG not defined */

#define USE_PRECOND_CONST

# define IFDEBUG(action)

# define POSTCOND(cond)
# define PRECOND(cond)
# define PRECOND_CONST(cond)

#endif

