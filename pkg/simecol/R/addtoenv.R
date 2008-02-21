## environment manipulation function
## Warning: use carefully and avoid name mangling
## solution provided by Gabor Grothendieck (replaces older own function)
## --> this function is deprecated and may be removed in further versions

addtoenv <- function(L, p = parent.frame()) {
  for(nm in names(L)) {
	  assign(nm, L[[nm]], p)
	  environment(p[[nm]]) <- p
  }
  L
}
