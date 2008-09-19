/* compile within R with system("R CMD SHLIB <filename>.cpp") */
/* Example adapted from lsoda documentation */

#include <R.h>

static double parms[3];

/* A typical C trick to get readable names for parameters */
#define k1 parms[0]
#define k2 parms[1]
#define k3 parms[2]

/* ~~~~~ here is the place for your C++ code   */


/* It is possible to define global variables here */
static double aGlobalVar = 99.99;   // for testing only

/* Functions that are called from R must be given in "C" and not C++ */
extern "C" {
  /* initializer: same name as the dll (without extension) */
  void lotka(void (* odeparms)(int *, double *)) {
      int N = 3;
      odeparms(&N, parms);
      Rprintf("lotka DLL is initialized\n");
  }
  /* Derivatives */
  void dlotka(int *neq, double *t, double *y, double *ydot, double *yout, int *ip) {
      // sanity checks
      if (ip[0] < 2) error("nout should be at least 2");
      // derivatives
      ydot[0] = k1 * y[0]        - k2 * y[0] * y[1];
      ydot[1] = k2 * y[0] * y[1] - k3 * y[1];
      
      // additional outputs, for demo purposes only
      yout[0] = aGlobalVar;
      yout[1] = y[1];    
  }
  
  /* Jacobian */
  //void jlotka(int *neq, double *t, double *y, int *ml,
  //           int *mu, double *pd, int *nrowpd, double *yout, int*ip) {
  //   // ~~~~~ optional: code for the Jacobian 
  //}

}
/* End of example */
 
