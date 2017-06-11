#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .C calls */
extern void eightneighbours(void *, void *, void *, void *);
extern void neighbours(void *, void *, void *, void *, void *, void *, void *, void *);
extern void seedfill(void *, void *, void *, void *, void *, void *, void *, void *);
extern void xneighbours(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"eightneighbours", (DL_FUNC) &eightneighbours, 4},
    {"neighbours",      (DL_FUNC) &neighbours,      8},
    {"seedfill",        (DL_FUNC) &seedfill,        8},
    {"xneighbours",     (DL_FUNC) &xneighbours,     9},
    {NULL, NULL, 0}
};

void R_init_simecol(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
