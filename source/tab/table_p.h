
#define LOW_DEFS   (             \
         (1<<TEST_ONE   )        \
	|(1<<TEST_GE_INF)        \
	|(1<<TEST_GT_INF)        \
	|(1<<TEST_GE_LE )        \
	|(1<<TEST_GT_LE )        \
	|(1<<TEST_GE_LT )        \
	|(1<<TEST_GT_LT ) )
#define LOW_DEFINED(t)	((1<<t) & LOW_DEFS)
#define HIGH_DEFINED(t)	((t) > 3)
