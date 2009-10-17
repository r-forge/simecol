`mcyst_batch` <-
function(){
  new("odeModel",
    main = function (time, init, parms, ...) {
      x <- init
      with(as.list(parms),{
        mu.inst <- mu * x[1] * (1-x[1]/K)
        dx1 <-  mu.inst      -  s * x[1]         # microcystis
        dx2 <-  mu.inst * p - (s + dM) * x[2]   # mcyst
        list(c(dx1, dx2))
      })
    },
    init = c(
      cells = 1e5,     # Inoculum    (cells/L)
      mcyst = 30 * 1e5 # Start-MCYST (fg/L)
    ),
    parms = c(
    	mu = 0.3,        # 1/d (Lab)
      K  = 1.0e7,      # Cells per ml
    	p  = 100,        # fg/cell
    	dM = 0.02,       # 1/d (Orr & Jones)
    	s  = 0.0         # sedimentation rate (assumption)
    ),
    times = c(from=0, to=100, by=1),
    solver = "lsoda"
  )
}

