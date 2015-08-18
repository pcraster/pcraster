#include "stddefx.h" 

/**************************************************************************/
/*  ran.c                                                                */ 
/*   implements random generator for values between 0 and 1               */
/*   documented in [Press 1986] as Ran3                                   */
/*   + GasDev() also in [Press 1986]                                      */
/**************************************************************************/

/********/
/* USES */
/********/
#include "mathx.h"


/***************/
/* EXTERNALS   */
/***************/

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 
#ifdef HAS_GETTIMEOFDAY
#error expected to be able to HAS_GETTIMEOFDAY symbol local here
#endif
static unsigned int MilliSecSeed(void);
static double next_random_number(void);
static void start_random_number (int seed_a, int seed_b);

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* Random number between 0 and 1
 * Ran() implements an uniform random generator.
 * The random generator currently used is the Marsaglia algorithm.
 * The returned range is [0,1> or exactly: 0.0 .. 1.0-2**(-24) inclusive.
 * Be aware that rounding problems can create a value of 1.
 * There are 2**24 possible return values.
 * Use SetRan() to initialize the random sequence.
 * Returns a number between 0 and 1 [0,1>.
 */
double Ran(void)
{
#ifdef DEBUG
  double d = next_random_number();
  POSTCOND( 0 <= d && d <= 1);
  return d;
#else
  return next_random_number();
#endif
}

#ifdef NEVER_DEF_THIS
static int ReadFromFile(void)
{
#ifndef DEBUG
#error ReadFromFile only possible in DEBUG mode
#endif
  int seed;
  FILE *f;
  static BOOL once=FALSE;
  if (!once)
    Warning("SEED READ FROM /tmp/SetRan\n");
  once = TRUE;
  PRECOND(FileStat("/tmp/SetRan")!=1);
  if (FileStat("/tmp/SetRan")==0)
  {
    f = fopen("/tmp/SetRan","r");
    POSTCOND(f!=NULL);
    POSTCOND(fscanf(f,"%d",&seed) > 0);
    fclose(f);
  }
  else
    seed = 0;
  seed++;
  f = fopen("/tmp/SetRan","w");
  POSTCOND(f!=NULL);
  POSTCOND(fprintf(f,"%d\n",seed) > 0);
  fclose(f);

  return seed;
}
#endif

static BOOL setRanCalled=FALSE;
/* Initialize Ran() once
 * checks if SetRan is ever called if not it calls SetRan with
 * value 0. InitRanOnce() garantuees that SetRan is only called
 * once
 */
void InitRanOnce(void)
{
  if (!setRanCalled) {
       SetRan(0);
  }
}

/* Initialize Ran()
 * SetRan initializes the random sequence given by Ran().
 * Each non-zero seed number has an unique sequence.
 *
 * TEST OPTION
 *
 * The mathx-library also contains a version of SetRan that
 * reads a number from the text file /tmp/SetRan if the seed parameter is
 * 0. The number is read (or assumed 0) if /tmp/SetRan is 
 * non-existent, first incremented, then used as seed and that
 * incremented number overwrites the old one in /tmp/SetRan 
 */
void SetRan(unsigned int seed) /* value >= 0. If 0 then seed is taken from
                                *  current time.
                                */
{
  unsigned int i;
  unsigned int a,b;

  setRanCalled=TRUE;

  if (seed == 0)
    /* MilliSecSeed or ReadFromFile
     */
    i = MilliSecSeed();
  else
    i = seed;
  a = ((i << 16) >> 16);
  b = i >> 16;
  (void)start_random_number((int)a,(int)b);
}

/* draw from standard normal distribution
 * First use SetRan to initialize.
 * Returns number drawn from normal distribution (mean = 0, sd = 1).
 */
double GasDev(void)
{
  static BOOL iset=FALSE;
  static double gset;
  double fac,r,v1,v2;

  if  (!iset) {
    do {
      v1=2.0*Ran()-1.0;
      v2=2.0*Ran()-1.0;
      r=v1*v1+v2*v2;
    } while (r >= 1.0 || r == 0.0);
    fac=sqrt(-2.0*log(r)/r);
    gset=v1*fac;
    iset=TRUE;
    return(v2*fac);
  }
  else
  {
    iset=FALSE;
    return(gset);
  }
}


#if _MSC_VER  ||  __TURBOC__
/* microsoft C 7.0 */
#include <sys\types.h>
#include <sys\timeb.h>

static unsigned int MilliSecSeed(void)
{

#ifdef _MSC_VER
 struct _timeb t;
 unsigned int sec;
 _ftime(&t);
#else
 struct timeb t;
 unsigned int sec;
 ftime(&t);
#endif
 sec =(unsigned int)t.time;
 sec *= 100;
 return(sec + (t.millitm/10));
}

#elif THINK_C
#ifndef MAC
#error THIS should be THINK C on the mac
#endif
#include <OSUtils.h> 
unsigned int MilliSecSeed(void)
{
 long time;
 unsigned int seed;

 GetDateTime(&time);     
 seed = (unsigned int)(time % UINT_MAX);
 return(seed);
}
#elif __linux__ 
# define HAS_GETTIMEOFDAY 1
#elif __APPLE__
# define HAS_GETTIMEOFDAY 1
#elif ALPHA_OSF 
# define HAS_GETTIMEOFDAY 1
#elif WIN32
#include <windows.h>
static unsigned int MilliSecSeed(void)
{

  /*
     the return value is the number of milliseconds that 
     have elapsed since Windows was started. 

    The elapsed time is stored as a DWORD value. Therefore, the time will wrap around to zero if Windows is run continuously for 49.7 days. 

    */
  return GetTickCount();
}


#elif __STDC__
     /* if not use function below */

#error defaulting to ANSI C seed initializing edit ran.c
/* Ansi C library does not return a current time in
 * millisecond, this MilliSecSeed returns nr of second
 * this is very dangerous when calling an app in a tight loop
 */
#include <time.h>
static unsigned int MilliSecSeed(void)
{
  time_t r;
  return((unsigned int)time(&r));
}
#else
#error NON_ANSI what about the seed?
#endif

#ifdef HAS_GETTIMEOFDAY
#include <sys/time.h>
#include <unistd.h>

/* assuming a 32 bit machine we have
 * UINT_MAX  4294967295
 * usec is 10^6 XXX1000000
 * take XXX (sec%429)
 */
static unsigned int MilliSecSeed(void)
{
  struct timeval tv;
  gettimeofday(&tv,0);
/*
  printf("TIME %u %u\n",(unsigned int)tv.tv_sec,(unsigned int)tv.tv_usec);
 */
  return ((((unsigned int)tv.tv_sec)%429)*1000000)
         +(unsigned int)tv.tv_usec;
}

#endif



#ifdef ___NEVER_DEF_NOT_NOT_USED
static const long Q1=13, Q2=2, S1=12,S2=17,P1mS1=19,P2mS2=12,P1mP2=2;
static const unsigned long Mask1=2147483647L, Mask2=536870911L;
static const double Norm=4.656612873e-10; 
static unsigned long I1=12345,I2=67890,b;

void SetTaus(
  unsigned long i1,
  unsigned long i2)
{
  I1 = i1; I2 = i2;
}

double TausComb(void)
{
  b = ((I1<<Q1)^I1) & Mask1;
  I1 = ((I1<<S1)^(b>>P1mS1)) & Mask1;
  b = ((I2<<Q2) ^ I2) & Mask2;
  I2 = ((I2<<S2) ^ (b>>P2mS2)) & Mask2;
  return((I1^(I2<< P1mP2))*Norm);
}

try SetRan(815219806), the 9th number will be neagtive

#define MBIG 1000000000L
#define MSEED 161803398L
#define MZ 0
#define FAC (1.0/MBIG)

static double Ran3(
  int *idum)
{
  static int inext,inextp;
  static long ma[56];
  static int iff=0;
  long mj,mk;
  int i,ii,k;

  if (*idum < 0 || iff == 0) {
    printf("init seed %d\n", *idum);
    iff=1;
    mj=MSEED-(*idum < 0 ? -*idum : *idum);
    mj %= MBIG;
    ma[55]=mj;
    mk=1;
    for (i=1;i<=54;i++) {
      ii=(21*i) % 55;
      ma[ii]=mk;
      mk=mj-mk;
      if (mk < MZ) mk += MBIG;
      mj=ma[ii];
    }
    for (k=1;k<=4;k++)
      for (i=1;i<=55;i++) {
        ma[i] -= ma[1+(i+30) % 55];
        if (ma[i] < MZ) ma[i] += MBIG;
      }
    inext=0;
    inextp=31;
    *idum=1;
  }
  if (++inext == 56) inext=1;
  if (++inextp == 56) inextp=1;
  mj=ma[inext]-ma[inextp];
  if (mj < MZ) mj += MBIG;
  ma[inext]=mj;
  return(mj*FAC);
}

#endif

/*
 *  Title:   random_number
 *  Last Mod:   Fri Mar 18 08:52:13 1988
 *  Author:   Vincent Broman
 *    <broman@schroeder.nosc.mil>
 */

#define P 179
#define PM1 (P - 1)
#define Q (P - 10)
#define STATE_SIZE 97
#define MANTISSA_SIZE 24
#define RANDOM_REALS 16777216.0
#define INIT_C    362436.0
#define INIT_CD  7654321.0
#define INIT_CM 16777213.0

static unsigned int ni;
static unsigned int nj;
static double u[STATE_SIZE];
static double c, cd, cm;


/*
 * return a value between 0 and size-1 inclusive.
 * this value will be anyint itself if possible, 
 * otherwise another value in the required interval.
 */
static unsigned int collapse (int anyint, unsigned int size)
{
    if (anyint < 0)
  anyint = - (anyint / 2);
    while ((unsigned int)anyint >= size)
  anyint /= 2;
    return (anyint);
}


/*
 * This procedure initialises the state table u for a lagged 
 * Fibonacci sequence generator, filling it with random bits 
 * from a small multiplicative congruential sequence.
 * The auxilliaries c, ni, and nj are also initialized.
 * The seeds are transformed into an initial state in such a way that
 * identical results are guaranteed across a wide variety of machines.
 */
static void start_random_number (int seed_a, int seed_b)
{
double s, bit;
unsigned int ii, jj, kk, mm;
unsigned int ll;
unsigned int sd;
unsigned int elt, bit_number;

    sd = collapse (seed_a, PM1 * PM1);
    ii = 1 + sd / PM1;
    jj = 1 + sd % PM1;
    sd = collapse (seed_b, PM1 * Q);
    kk = 1 + sd / PM1;
    ll = sd % Q;
    if (ii == 1 && jj == 1 && kk == 1)
  ii = 2;

    ni = STATE_SIZE - 1;
    nj = STATE_SIZE / 3;
    c  = INIT_C;
    c /= RANDOM_REALS;    /* compiler might mung the division itself */
    cd = INIT_CD;
    cd /= RANDOM_REALS;
    cm = INIT_CM;
    cm /= RANDOM_REALS;

    for (elt = 0; elt < STATE_SIZE; elt += 1) {
  s = 0.0;
  bit = 1.0 / RANDOM_REALS;
  for (bit_number = 0; bit_number < MANTISSA_SIZE; bit_number += 1) {
      mm = (((ii * jj) % P) * kk) % P;
      ii = jj;
      jj = kk;
      kk = mm;
      ll = (53 * ll + 1) % Q;
      if (((ll * mm) % 64) >= 32)
    s += bit;
      bit += bit;
  }
  u[elt] = s;
    }
}
    
    
/*
 * Return a uniformly distributed pseudo random number
 * in the range 0.0 .. 1.0-2**(-24) inclusive.
 * There are 2**24 possible return values.
 * Side-effects the non-local variables: u, c, ni, nj.
 */
static double next_random_number(void)
{
double uni;
        
    if (u[ni] < u[nj])
  uni = u[ni] + (1.0 - u[nj]);
    else
  uni = u[ni] - u[nj];
    u[ni] = uni;

    if (ni > 0)
  ni -= 1;
    else
  ni = STATE_SIZE - 1;

    if (nj > 0)
  nj -= 1;
    else
  nj = STATE_SIZE - 1;

    if (c < cd)
  c = c + (cm - cd);
    else
  c = c - cd;

    if (uni < c)
  return (uni + (1.0 - c));
    else
  return (uni - c);
}

#ifdef NEVER_DEF_THIS
void main(void)
{
  double r;
  int i;
  SetRan(0);
  for(i=0; i < 10000; i++)
  {
    r = Ran();
    if (r < 0 || r > 1 || i == 0)
      printf("   %d wrong %20.10f\n",i,r);
  }

}
#endif

/*   from rn.h:
 *  This package makes available Marsaglia's highly portable generator 
 *  of uniformly distributed pseudo-random numbers.
 *
 *  The sequence of 24 bit pseudo-random numbers produced has a period 
 *  of about 2**144, and has passed stringent statistical tests 
 *  for randomness and independence.
 *  
 *  Supplying two seeds to start_random_number is required once
 *  at program startup before requesting any random numbers, like this:
 *      start_random_number(101, 202);
 *      r := next_random_number();
 *  The correspondence between pairs of seeds and generated sequences 
 *  of pseudo-random numbers is many-to-one.
 *  
 *  This package should compile and run identically on any 
 *  machine/compiler which supports >=16 bit integer arithmetic
 *  and >=24 bit floating point arithmetic.
 *  
 *  References:
 *      M G Harmon & T P Baker, ``An Ada Implementation of Marsaglia's
 *      "Universal" Random Number Generator'', Ada Letters, late 1987.
 *
 *      G Marsaglia, ``Toward a universal random number generator'',
 *      to appear in the Journal of the American Statistical Association.
 *
 *  George Marsaglia is at the Supercomputer Computations Research Institute
 *  at Florida State University.
 */
