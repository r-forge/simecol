#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .C calls */
extern void c_eightneighbours(void *, void *, void *, void *);
extern void c_neighbours(void *, void *, void *, void *, void *, void *, void *, void *);
extern void c_seedfill(void *, void *, void *, void *, void *, void *, void *, void *);
extern void c_xneighbours(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"c_eightneighbours", (DL_FUNC) &c_eightneighbours, 4},
    {"c_neighbours",      (DL_FUNC) &c_neighbours,      8},
    {"c_seedfill",        (DL_FUNC) &c_seedfill,        8},
    {"c_xneighbours",     (DL_FUNC) &c_xneighbours,     9},
    {NULL, NULL, 0}
};

void R_init_simecol(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
