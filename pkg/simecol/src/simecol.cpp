/* 
   Helper functions for the simecol package
   currently seedfill of bitmapped images,
   neighbour functions for cellular automata
   Th. Petzoldt

   non-recursive seedfill implemented
   using the algorithm of
   http://alumni.imsa.edu/~stendahl/comp/src/fill.c
*/

#include <cmath>
#include <Rinternals.h>

extern "C" {

int imax(int x, int y) {
    if (x > y) {
      return x;
    } else {
      return y;
    }
}

int imin(int x, int y) {
    if (x < y) {
      return x;
    } else {
      return y;
    }
}

bool is_inside(int n, int m, int i, int j, double* x) {
    if ((0 <= i) & (i < n) & (0 <= j) & (j < m))  return true; else return false;
}

double getpixel(int n, int m, int i, int j, double* x) {
    if (is_inside(n, m, i, j, x)) {
      return x[i + n * j];
    } else {
      return 0;
    }
}

//version 2: return bound color if outside the area
double getpixelb(int n, int m, int i, int j, double* x, double bound) {
    if (is_inside(n, m, i, j, x)) {
      return x[i + n * j];
    } else {
      return bound;
    }
}

void setpixel(int n, int m, int i, int j, double* x, double* fcol) {
    if (is_inside(n, m, i, j, x)) {
       x[i + n * j] = *fcol;
    }
}
 

// recursive version of seedfill
void fill(int* n, int* m, int* i, int* j, double* x, 
          double* fcol, double* bcol, double* tol) {
  int ii=*i, jj=*j; double col;
  if (is_inside(*n, *m, *i, *j, x)) {
    col=getpixel(*n, *m, *i, *j, x);

    if( col != *fcol && col != *bcol ) {
      setpixel(*n, *m, *i, *j, x, fcol);
      ii=*i+1; fill(n, m,    &ii, j,   x, fcol, bcol, tol);
      jj=*j+1; fill(n, m,    i,   &jj, x, fcol, bcol, tol);
      ii=*i-1; fill(n, m,    &ii, j,   x, fcol, bcol, tol);
      jj=*j-1; fill(n, m,    i,   &jj, x, fcol, bcol, tol);
    }
  }
}  

// non-recursive seedfill
void pushSeed(int x, int y, int* xstack, int* ystack, int* ptr, int maxptr) {
  xstack[*ptr] = x;
  ystack[*ptr] = y;
  *ptr = *ptr + 1;
  if (*ptr > maxptr) 
    error("fatal error in package simecol: stack size exceeded in seedfill");
}

bool popSeed(int* x, int* y, int* xstack, int* ystack, int* ptr) {
  bool ret = false;
  if (*ptr > 0) {
    *ptr = *ptr - 1;
    *x = xstack[*ptr];
    *y = ystack[*ptr];
    ret = true;
  }
  return ret;    
}

// fill pixels to the left and right of the seed pixel until you hit 
// boundary pixels.  Return the locations of the leftmost and rightmost 
// filled pixels.
void FillContiguousSpan(int x, int y, double bound, double fill, int *xLeft, int *xRight,
                        int n, int m, double* xx, double tol) {
   double col;
   int i;
   // fill pixels to the right until you reach a boundary pixel
   i = x;
   col = getpixelb(n, m, i, y, xx, bound);
   while (fabs(col - bound) > tol) {
      setpixel(n, m, i, y, xx, &fill);
      i++;
      col = getpixelb(n, m, i, y, xx, bound);
   }
   *xRight = i-1;
   // fill pixels to the left until you reach a boundary pixel
   i = x-1;
   col = getpixelb(n, m, i, y, xx, bound);
   while(fabs(col - bound) > tol) {
      setpixel(n, m, i, y, xx, &fill);
      i--;
      col = getpixelb(n, m, i, y, xx, bound);
   }
   *xLeft = i+1;
}


// the main routine
void FillSeedsOnStack(double bound, double fill, 
                      int n, int m, double* xx,
                      int* xstack, int* ystack, int* ptr, int maxptr, double tol) {
   double col1=0, col2=0;
   int x, y;              // current seed pixel
   int xLeft, xRight;     // current span boundary locations 
   int i;

   while (popSeed(&x, &y, xstack, ystack, ptr)) {
      if (fabs(getpixelb(n, m, x, y, xx, bound) - bound) > tol) {
         FillContiguousSpan(x, y, bound, fill, &xLeft, &xRight, n, m, xx, tol);
         // single pixel spans handled as a special case in the else clause
         if (xLeft != xRight) {
            // handle the row above you
            y++;
            for(i=xLeft+1; i<=xRight; i++) {
               col1 = getpixelb(n, m, i-1, y, xx, bound);
               col2 = getpixelb(n, m, i,   y, xx, bound);
               if (fabs(col1 - bound) > tol && fabs(col1 - fill) > tol 
                                            && fabs(col2 - bound) <= tol)
                  pushSeed(i-1, y, xstack, ystack, ptr, maxptr);
            }
            if (fabs(col2 - bound) > tol && fabs(col2 - fill) > tol)
               pushSeed(xRight, y, xstack, ystack, ptr, maxptr); 

            // handle the row below you
            y -= 2;
            for(i=xLeft+1; i<=xRight; i++) {
               col1 = getpixelb(n, m, i-1, y, xx, bound);
               col2 = getpixelb(n, m, i,   y, xx, bound);
               if (fabs(col1 - bound) > tol && fabs(col1 - fill) > tol 
                                            && fabs(col2 - bound) <= tol)
                  pushSeed(i-1, y, xstack, ystack, ptr, maxptr);
            }
            if (fabs(col2 - bound) > tol && fabs(col2 - fill) > tol)
               pushSeed(xRight, y, xstack, ystack, ptr, maxptr); 
         } else {
            col1 = getpixelb(n, m, xLeft, y+1, xx, bound);
            col2 = getpixelb(n, m, xLeft, y-1, xx, bound);
            if (fabs(col1 - fill) > tol)
               pushSeed(xLeft, y+1, xstack, ystack, ptr, maxptr);
            if (fabs(col2 - fill) > tol)
               pushSeed(xLeft, y-1, xstack, ystack, ptr, maxptr);
         }

      } // end if (GetPixel)
   }  // end while (popSeed)
}

// start routine for seedfill
void seedfill(int* n, int* m, int* i, int* j, double* x, 
              double* fcol, double* bcol, double* tol) {
  int* xstack;
  int* ystack;
  int p=0, *ptr;
  int maxptr;
  xstack = (int *) R_alloc(*n * *m, sizeof(int)); // sorry. estimated value only 
  ystack = (int *) R_alloc(*n * *m, sizeof(int));
  maxptr = *m * *n;
  ptr = &p;
  pushSeed(*i, *j, xstack, ystack, ptr, maxptr);
  FillSeedsOnStack(*bcol, *fcol, *n, *m, x, xstack, ystack, ptr, maxptr, *tol);
}

// basic neighbourhood function for Conway's Game of Life
void eightneighbours(int* n, int* m, double* x, double* y) {
  int nn= *n, mm= *m;
  double c=0;
  for (int i=0; i < nn; i++) {
      for (int j=0; j < mm; j++) {
        c = getpixel(nn, mm, i+1, j,   x) +
            getpixel(nn, mm, i,   j+1, x) +
            getpixel(nn, mm, i-1, j,   x) +
            getpixel(nn, mm, i,   j-1, x) +
            getpixel(nn, mm, i+1, j+1, x) +
            getpixel(nn, mm, i+1, j-1, x) +
            getpixel(nn, mm, i-1, j+1, x) +
            getpixel(nn, mm, i-1, j-1, x);
        setpixel(nn, mm, i, j, y, &c);
      }
  }
}

// generalized neighbourhood function for cellular automata
void neighbours(int* n, int* m, double* x, double* y, 
                int* ndist, double* wdist, double* state, double* tol) {
  // n = number of rows in grid
  // m = number of columns in grid
  // x = input grid matrix
  // y = output grid matrix
  // ndist = number of rows and columns in distance matrix
  // wdist = weights of distance matrix
  // state = value to check for
  // tol   = tolerance when comparing states
  int   nn = *n, mm = *m, nd = *ndist, d;
  double s = 0,  c = 0, dstate = *state, dtol = *tol;

  d = (int)floor(*ndist / 2); 
  for (int i=0; i < nn; i++) {
    for (int j=0; j < mm; j++) {
      c=0;
      for (int ii= imax(-d, -i); ii <= imin(nn-i, d); ii++) {
	for (int jj= imax(-d, -j); jj <= imin(mm-j, d); jj++) {
          s = getpixel(nn, mm, i + ii, j + jj,   x);
          if (fabs(s - dstate) < dtol) {
            c += wdist[ii + d + nd * (jj + d)];
          }
        }
      }
      setpixel(nn, mm, i, j, y, &c);
    }
  }
}

} // End extern "C"

