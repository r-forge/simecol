!##########################################################################
! OMEXDIA - Model describing the dynamics of carbon, nitrogen and oxygen
! in a marine sediment. (Soetaert et al., 1996 a,b).
!
! Soetaert K, PMJ Herman and JJ Middelburg, 1996.
! A model of early diagenetic processes from the shelf to abyssal depths.
! Geochim. Cosmochim. Acta, 60(6):1019-1040.
! Soetaert K, PMJ Herman and JJ Middelburg, 1996b.
! Dynamic response of deep-sea sediments to seasonal variation: a model.
! Limnol. Oceanogr. 41(8): 1651-1668.
!
! The model describes six state variables, in 200 layers:
!
! fast decaying organic carbon (FDET), solid substance (nmol/cm3 solid)
! slow decaying organic carbon (SDET), solid substance
! Oxygen (O2), dissolved substance (nmol/cm3 liquid)
! Nitrate (NO3), dissolved substance
! Ammonium (NH3), dissolved substance
! Oxygen demand unit (ODU), dissolved,
!                    =lump sum of reduced substances other than ammonium
!##########################################################################


!==========================================================================
!==========================================================================
! initialisation subroutine: 
! initialises the common block with 25 parameter values, 
! followed by thicknesses, porosities, bioturbation values
!==========================================================================
!==========================================================================

       subroutine initomexdia (steadyparms)
       external steadyparms
       integer,parameter :: N=200
       integer,parameter :: nc = 2*N + 3*(N+1) + 25

       double precision parms(nc)
       common /myparms/parms

       call steadyparms(nc, parms)

       return
       end


!==========================================================================
!==========================================================================
! subroutine calculating the rate of change
! the omexdia model
!==========================================================================
!==========================================================================

      subroutine omexdiamod (neq, t, Conc, dConc, yout, ip)
      
      implicit none


!......................... declaration section.............................
      integer           :: neq, ip(*), i
      integer,parameter :: N=200         
      double precision  :: t, Conc(*), dConc(*), yout(*), Flux(N+1)
      double precision :: por(N),intpor(N+1),Db(N+1),dx(N),dxInt(N+1)
      double precision :: Fdet(N),Sdet(N),O2(N),NO3(N),NH3(N),ODU(N)
      double precision :: dFdet(N),dSdet(N),dO2(N),dNO3(N),               &
     &                     dNH3(N),dODU(N)

      double precision :: cflux,Cprod(N),Nprod(N),Rescale(N)
      double precision :: Oxicminlim(N),Denitrilim(N),Anoxiclim(N) 
      double precision :: Oxicmin(N),Denitrific(N),anoxicmin(N)
      double precision :: nitri(N),oduox(N),odudepo(N)
      double precision :: pdepo

       
      double precision :: MeanFlux,rFast,rSlow ,pFast,w,NCrFdet,          & 
     &  NCrSdet,bwO2,bwNO3,bwNH3,bwODU,NH3Ads,rnit,ksO2nitri,             &
     &  rODUox,ksO2oduox,ksO2oxic,ksNO3denit,kinO2denit,kinNO3anox,       &
     &  kinO2anox,DispO2,DispNO3,DispNH3,DispODU   

      common /myparms    /MeanFlux,rFast,rSlow ,pFast,w,NCrFdet,          & 
     &  NCrSdet,bwO2,bwNO3,bwNH3,bwODU,NH3Ads,rnit,ksO2nitri,             &
     &  rODUox,ksO2oduox,ksO2oxic,ksNO3denit,kinO2denit,kinNO3anox,       &
     &  kinO2anox,DispO2,DispNO3,DispNH3,DispODU,dx,dxint,                &
     &  por,intpor,Db   

      double precision :: integrate
!............................ statements ..................................
       if (IP(1) < 9)  call rexit("nout should be at least 9")

! from Conc to fdet, sdet, o2,...
       do i = 1,N 
          Fdet(i) = Conc(i)
          Sdet(i) = Conc(i+N)          
          O2  (i) = Conc(i+2*N)          
          NO3 (i) = Conc(i+3*N)                              
          NH3 (i) = Conc(i+4*N)          
          ODU (i) = Conc(i+5*N)          
       enddo

      CFlux  = MeanFlux * (1+sin(2.*3.14158*t/365.))
      yout(1) = CFlux
	 
! Rate of change due to transport: solid substances
      CALL tran1Dsol(FDET,cFlux*pFast,Db,w,por,intpor,dx,dxint,            &
     &               Flux,dFDET)

      CALL tran1Dsol(SDET,cFlux*(1.-pFast),Db,w,por,intpor,dx,dxint,       &
     &               Flux,dSDET)

! Rate of change due to transport: solute substances
      CALL tran1dliq(O2 ,bwO2 ,dispO2 ,w,por,intpor,dx,dxint,Flux,dO2)     
      yout(2) = Flux(1) ! sediment-water O2 flux

      CALL tran1dliq(NH3,bwNH3,dispNH3/(1+NH3Ads),w,por,intpor,dx,dxint,   &
     &               Flux,dNH3)                        
      yout(3) = Flux(1)

      CALL tran1dliq(NO3,bwNO3,dispNO3,w,por,intpor,dx,dxint,Flux,dNO3)                        
      yout(4) = Flux(1)

      CALL tran1dliq(ODU,bwODU,dispODU,w,por,intpor,dx,dxint,Flux,dODU)                        
      yout(5) = Flux(1)

! Production of DIC and DIN, expressed per cm3 LIQUID/day
      Cprod= (rFast*FDET        +rSlow*SDET        )*(1.-por)/por
      Nprod= (rFast*FDET*NCrFdet+rSlow*SDET*NCrSdet)*(1.-por)/por

! Oxic mineralisation, denitrification, anoxic mineralisation
! first the limitation terms
      Oxicminlim = O2/(O2+ksO2oxic)                ! limitation terms
      Denitrilim = (1.-O2/(O2+kinO2denit))*NO3/(NO3+ksNO3denit)
      Anoxiclim  = (1.-O2/(O2+kinO2anox))*(1-NO3/(NO3+kinNO3anox))
      Rescale    = 1./(Oxicminlim+Denitrilim+Anoxiclim)

! then the mineralisation rates
      OxicMin    = Cprod*Oxicminlim*Rescale        ! oxic mineralisation
      Denitrific = Cprod*Denitrilim*Rescale        ! Denitrification
      AnoxicMin  = Cprod*Anoxiclim *Rescale        ! anoxic mineralisation

! reoxidation and ODU deposition
      Nitri      = rnit  *NH3*O2/(O2+ksO2nitri)
      OduOx      = rODUox*ODU*O2/(O2+ksO2oduox)

      pDepo      = min(1.d0,0.233*(w*365)**0.336 )
      OduDepo    = AnoxicMin*pDepo 

! Update the rate of change with biogeochemical processes
      dFDET = dFDET  - rFast*FDET
      dSDET = dSDET  - rSlow*SDET
      dO2   = dO2    - OxicMin        -2* Nitri -      OduOx
      dNH3  = dNH3   + (Nprod           - Nitri) / (1.+NH3Ads)
      dNO3  = dNO3   - 0.8*Denitrific   + Nitri
      dODU  = dODU   + AnoxicMin                - OduOx - OduDepo

! from dfdet, dsdet, do2,... to dconc
       do i = 1,N
          dConc(i)   = dfdet(i) 
          dConc(i+N) = dsdet(i) 
          dConc(i+2*N) = dO2(i) 
          dConc(i+3*N) = dNO3(i) 
          dConc(i+4*N) = dNH3(i) 
          dConc(i+5*N) = dODU(i) 
       enddo

! the integrated rates
! Total mineralisation
      yout(6) = integrate(Cprod,dx,por,N)
! Total oxic mineralisation
      yout(7) = integrate(OxicMin,dx,por,N)
! Total denitrificatin
      yout(8) = integrate(Denitrific,dx,por,N)
! Total nitrification
      yout(9) = integrate(Nitri,dx,por,N)
      return
      end

!==========================================================================
! integrated quantities;
! solid substance y is in mmol/ m3 solid
! liquid substance y is in mmol /m3 liquid.
! converted to mmol/m3 bulk (* 1-por) or *(por) and integrate; porfac is
!==========================================================================

      double precision function integrate(y,dx,porfac,N)
      integer          :: i,N
      double precision :: y(N), dx(N), porfac(N)

      integrate = 0.d0
      do i = 1, N
       integrate = integrate + y(I)*dx(I)*porfac(I)
      enddo       

      end
       
!==========================================================================
! transport of liquids
!==========================================================================

       subroutine tran1dliq(y,yup,disp,v,por,porint,dx,dxint,Flux,dy)

       integer,parameter :: N=200         
       double precision, intent(in):: y(N),disp,v,por(N),porint(N+1)
       double precision, intent(in):: dx(N),dxint(N+1),yup 
       double precision, intent(out):: flux(N+1),dy(N)
       integer :: i
       double precision :: ydown,porfac(N+1)

! boundaries
       ydown = y(N)       

! advection, corrected for porosity
         do i = 1,N+1
           porfac(i) = porint(N+1)/porint(i)
         ENDDO

! dispersion flux  
         do i =1,N-1
           Flux(i+1) = (y(i)-y(i+1))/dxint(i+1)
         enddo
         Flux(1)     = (yup-y(1))/dxint(1)  
         Flux(N+1)   = (y(N)-ydown)/dxint(N+1)
         do i =1,N+1
          Flux(i) = Disp*Flux(i)
         enddo 

! advection flux in direction of axis - centered differences
         do i =2,N+1
           Flux(i) = Flux(i) + 0.5*v*porfac(i)*y(i-1)
         enddo
         Flux(1) = Flux(1) + 0.5*v*porfac(1)*yup

         do i =1,N
           Flux(i) = Flux(i)   + 0.5*v*porfac(i)*y(i)
         enddo
         Flux(N+1) = Flux(N+1) + 0.5*v*porfac(N+1)*ydown

! Fluxes per bulk
       Flux = Flux * porint

! rate of change = flux gradient, corrected for surface (porosity) changes 
       do i =1,N
         dy(i) = (Flux(i)-Flux(i+1))  /por(i)/dx(i)
       enddo
       
       end subroutine

!==========================================================================
! transport of solids
!==========================================================================

       subroutine tran1dsol(y,fluxup,disp,v,por,porint,dx,dxint,Flux,dy)

       integer,parameter :: N=200         
       double precision, intent(in):: y(N),fluxup,disp(N+1),v,por(N)
       double precision, intent(in):: dx(N),dxint(N+1),porint(N+1)
       double precision, intent(out):: flux(N+1),dy(N)
       integer :: i
       double precision :: yup,ydown,porfac(N+1)
       character(len=80) msg

! boundaries
       yup   = y(1)       
       ydown = y(N)       

! dispersion flux  
         do i =1,N-1
           Flux(i+1) = (y(i)-y(i+1))/dxint(i+1)
         enddo

         Flux(N+1)   = (y(N)- ydown)/dxint(N+1)
         do i =1,N+1
          Flux(i) = Disp(i)*Flux(i)
         enddo 
! advection, corrected for porosity
         do i = 1,N+1
           porfac(i) = (1.-porint(N+1))/(1.-porint(i))
         ENDDO
! advection flux in direction of axis
         do i =2,N+1
           Flux(i) = Flux(i) + 0.5*v*porfac(i)*y(i-1)
         enddo

         do i =1,N
           Flux(i) = Flux(i)     + (1.-0.5)*v*porfac(i)*y(i)
         enddo
         Flux(N+1) = Flux(N+1) + (1.-0.5)*v*porfac(N+1)*ydown

! fluxes per bulk
       Flux = Flux * (1-porint)

! upper boundary flux
       Flux(1) = fluxup
              
! rate of change = flux gradient, corrected for surface (porosity) changes 
       do i =1,N
         dy(i) = (Flux(i)-Flux(i+1)) /(1.-por(i))/dx(i)         
       enddo
       
       end subroutine
       
