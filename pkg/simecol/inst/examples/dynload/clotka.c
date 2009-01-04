/* How to write a dynamic model in C
   Example: Lotka-Volterra Model
   Implementation adapted from lsoda example and documentation
   http://cran.r-project.org/web/packages/deSolve/vignettes/compiledCode.pdf
   compile within R with system("R CMD SHLIB clotka.c")
*/

#include <R.h>

/* A typical C trick to get readable names for the parameters is #define
   This method is simple and efficient, but there are, of course,
   other possibilities that use dynamic variables.
*/
static double parms[3];

#define k1 parms[0]
#define k2 parms[1]
#define k3 parms[2]

/* It is possible to define global variables here */
static double aGlobalVar = 99.99;   // for testing only

/* initializer: same name as the dll (without extension) */
void clotka(void (* odeparms)(int *, double *)) {
    int N = 3;
    odeparms(&N, parms);
    Rprintf("model parameters succesfully initialized\n");
}

/* Derivatives */
void dlotka(int *neq, double *t, double *y, double *ydot, double *yout, int *ip) {
    // sanity checks
    if (ip[0] < 2) error("nout should be at least 2");
    // derivatives
    ydot[0] = k1 * y[0]        - k2 * y[0] * y[1];
    ydot[1] = k2 * y[0] * y[1] - k3 * y[1];
    
    // additional outputs, here for demo purposes only
    yout[0] = aGlobalVar;
    yout[1] = ydot[0];
}

/* It is also possible to orivide a Jacobian (otionally) */
//void jlotka(int *neq, double *t, double *y, int *ml,
//           int *mu, double *pd, int *nrowpd, double *yout, int*ip) {
//   // ~~~~~ optional: code for the Jacobian 
//}


