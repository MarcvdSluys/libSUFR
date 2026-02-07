!> \file solvers.f90  Procedures to solve equations

   
!  Copyright (c) 2002-2025  Marc van der Sluys - Nikhef/Utrecht University - marc.vandersluys.nl
!   
!  This file is part of the libSUFR package, 
!  see: http://libsufr.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the European Union
!  Public Licence 1.2 (EUPL 1.2).  This software is distributed in the hope that it will be useful, but
!  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
!  PURPOSE.  See the EU Public Licence for more details.  You should have received a copy of the European
!  Union Public Licence along with this code.  If not, see <https://www.eupl.eu/1.2/en/>.
!  
!  




!***********************************************************************************************************************************
!> \brief  Procedures to solve equations

module SUFR_solvers
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Using Brent's method, find the root of a function func that lies between x1 and x2.
  !!
  !! \param func  Function to find the root of
  !! \param x1    Lower limit in x for root: x1 < root < x2;  func(x1) and func(x2) must be positive and negative or vice versa
  !! \param x2    Upper limit in x for root: x1 < root < x2;  func(x1) and func(x2) must be positive and negative or vice versa
  !! \param accur  The accuracy with which the root is to be determined
  !! 
  !! \param status   Status: 0-ok, 1-maximum number of iterations exceeded, 2-root not bracketed  (output, optional)
  !! \param verbose  Verbosity: 0-print nothing, 1-print errors, 2-print warnings, 3-print info  (output, optional, default=2)
  !! 
  !! \retval root_solver  The value of the root of func, between x1 and x2 and with accuracy accur.  If a root was not bracketed by (output)
  !!                      x1 and x2, -huge is returned and status=1.
  !! \see Numerical Recipes in Fortran 77, Sect.9.3.
  
  function root_solver(func,x1,x2,accur, status,verbose)
    use SUFR_kinds, only: double, dbl
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: x1,x2,accur
    integer, intent(in), optional :: verbose
    integer, intent(out), optional :: status
    
    real(double) :: root_solver
    real(double), external :: func
    
    integer, parameter :: max_iter = 100                            ! Maximum allowed number of iterations
    real(double), parameter :: eps = epsilon(0.0_dbl)               ! Machine precision
    
    integer :: iter,verbosity
    real(double) :: a,b,c,d,e, fa,fb,fc
    real(double) :: p,q,r,s, accur1,xm
    
    if(present(status)) status = 0
    verbosity = 2
    if(present(verbose)) verbosity = verbose
    
    a = x1
    b = x2
    fa = func(a)
    fb = func(b)
    
    !Prevent compiler complaints:
    d =  1.0_dbl
    e =  2.0_dbl
    
    if(fa*fb .gt. 0.0_dbl) then                                     ! func(a) and func(b) have the same sign
       if(verbosity.gt.0) write(0,'(2(A,2ES12.3))') ' libSUFR - root_solver():  root is not bracketed by x1 and x2: ', &
            x1,x2,' - ',fa,fb
       root_solver = -huge(0.0_dbl)                                 ! return -huge: the smallest number for this kind
       if(present(status)) status = 2
       return
    end if
    
    c = b
    fc = fb
    
    do iter = 1,max_iter
       if(fb*fc.gt.0.0_dbl) then                                    ! func(b) and func(c) have the same sign
          c = a
          fc = fa
          d = b-a
          e = d
       end if
       
       if(abs(fc).lt.abs(fb)) then
          a = b
          b = c
          c = a
          fa = fb
          fb = fc
          fc = fa
       end if
       
       accur1 = 2*eps*abs(b) + 0.5_dbl*accur
       xm = 0.5_dbl*(c-b)
       
       if(abs(xm).le.accur1 .or. deq(fb,0.0_dbl)) then          ! Then we have a sufficiently accurate solution
          root_solver = b
          return
       end if
       
       if(abs(e).ge.accur1 .and. abs(fa).gt.abs(fb)) then
          s = fb/fa
          if(deq(a,c)) then
             p = 2*xm*s
             q = 1.0_dbl - s
          else
             q = fa/fc
             r = fb/fc
             p = s * ( 2*xm*q*(q-r) - (b-a)*(r-1.0_dbl) )
             q = (q-1.0_dbl) * (r-1.0_dbl) * (s-1.0_dbl)
          end if
          
          if(p.gt.0.0_dbl) q = -q
          p = abs(p)
          
          if(2*p .lt. min(3*xm*q-abs(accur1*q),abs(e*q))) then
             e = d
             d = p/q
          else
             d = xm
             e = d
          end if
       else
          d = xm
          e = d
       end if
       
       a = b
       fa = fb
       
       if(abs(d) .gt. accur1) then
          b = b + d
       else
          b = b + sign(accur1,xm)
       end if
       
       fb = func(b)                                                 ! Evaluate the function func
       
    end do
    
    
    if(verbosity.gt.0) write(0,'(A)') ' libSUFR - root_solver():  maximum number of iterations exceeded'
    if(present(status)) status = 1
    
    root_solver = b
    
  end function root_solver
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Use Brent's method and parabolic interpolation to find the minimum of function func that lies between xlow and xhigh.
  !!
  !! \param func       The function of which the minimum is to be found
  !! \param xlow       The lower limit for the x-value of the minimum:  xlow < x_min < xhigh
  !! \param xguess     A good guess for the x-value of the minimum:  xlow < xb < xhigh  and  func(xb) < min(funx(xlow),func(xhigh))
  !! \param xhigh      The upper limit for the x-value of the minimum:  xlow < x_min < xhigh
  !! \param accur      Relative accuracy with which the minimum is to be found
  !! 
  !! \param xmin       X-value of the minimum (output)
  !! \param status     Status: 0-ok, 1-range has zero width, 2-initial guess outside range,
  !!                           5-minimum outside range, 9-maximum number of iterations exceeded  (output, optional)
  !! \param verbosity  Verbosity: 0-print nothing, 1-print errors, 2-print warnings, 3-print info
  !!                   (output, optional argument, default=2)
  !! 
  !! \retval minimum_solver  The value of the minimum of the function func, to accuracy accur
  !!
  !! \see Numerical Recipes in Fortran 77, Sect.10.2.
  
  function minimum_solver(func, xlow,xguess,xhigh, accur,xmin, status,verbosity)
    use SUFR_kinds, only: double, dbl
    use SUFR_system, only: error
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: xlow,xguess,xhigh,accur
    real(double), intent(out) :: xmin
    integer, intent(out), optional :: status
    integer, intent(in), optional :: verbosity
    
    real(double) :: minimum_solver
    real(double), external :: func
    
    integer, parameter :: max_iter = 100
    real(double), parameter :: eps = epsilon(0.0_dbl)  ! Machine precision
    character(len=*), parameter :: myname = 'LibSUFR/solvers/minimum_solver()'
    
    integer :: iter,verbosityl
    real(double) :: xlowl,xhighl, dd,ee,ee1, pp,qq,rr, accur1,accur2
    real(double) :: xval,xnew, fxval,fxnew, xmean,xrange, vv,ww, fv,fw
    
    
    ! Optional parameters:
    if(present(status)) status = 0
    verbosityl = 2
    if(present(verbosity)) verbosityl = verbosity
    
    ! Local versions of dummy parameters:
    xlowl = min(xlow, xhigh)
    xhighl = max(xlow, xhigh)
    
    minimum_solver = -huge(xguess)
    
    ! Check input:
    if( deq(xlowl,xhighl) ) then
       if(verbosityl.gt.0) call error(trim(myname)//': specified range has zero width')
       if(present(status)) status = 1
       return
    end if
    if( (xguess.lt.xlowl) .or. (xguess.gt.xhighl)) then
       if(verbosityl.gt.0) call error(trim(myname)//': initial guess not within specified range')
       if(present(status)) status = 2
       return
    end if
    
    
    ! Initialise variables:
    xval = xguess
    vv = xguess
    ww = xguess
    
    fxval = func(xval)
    fv = fxval
    fw = fxval
    
    ! Prevent compiler complaints:
    dd = huge(0.0_dbl)
    ee = huge(0.0_dbl)
    
    
    accur1 = accur + eps                                                    ! Use absolute accuracy
    accur2 = 2*accur1
    
    do iter=1,max_iter
       xmean = 0.5_dbl*(xlowl+xhighl)
       
       if( abs(xval-xmean) .le. accur2 - 0.5_dbl*(xhighl-xlowl) ) then       ! Then we have a sufficiently accurate solution
          xmin = xval
          xrange = abs(xhigh-xlow)
          if( (abs(xmin-xlow) .lt. xrange*1.d-9) .or. (abs(xmin-xhigh) .lt. xrange*1.d-9) ) then
             if(verbosityl.gt.0) call error(trim(myname)//': minimum not within specified range')
             if(present(status)) status = 5
          end if
          
          minimum_solver = fxval
          return
       end if
       ! **************************************************************************************************
       
       if(abs(ee).gt.accur1) then
          rr = (xval-ww) * (fxval-fv)
          qq = (xval-vv) * (fxval-fw)
          pp = (xval-vv)*qq - (xval-ww)*rr
          qq = 2*(qq-rr)
          if(qq.gt.0.0_dbl) pp = -pp
          qq = abs(qq)
          ee1 = ee
          ee = dd
          
          if(abs(pp).ge.abs(0.5_dbl*qq*ee1) .or. pp.le.qq*(xlowl-xval) .or. pp.ge.qq*(xhighl-xval)) then
             call golden_section(xval,xmean, xlowl,xhighl, dd,ee)                                         ! Do a golden-section step
          else
             dd = pp/qq
             xnew = xval + dd
             if(xnew-xlowl.lt.accur2 .or. xhighl-xnew.lt.accur2) dd = sign(accur1,xmean-xval)
          end if
       else
          call golden_section(xval,xmean, xlowl,xhighl, dd,ee)                                            ! Do a golden-section step
       end if
       
       ! Determine the point xnew where func must be evaluated next:
       if(abs(dd).ge.accur1) then
          xnew = xval + dd
       else
          xnew = xval + sign(accur1,dd)
       end if
       
       
       fxnew = func(xnew)
       
       if(fxnew.le.fxval) then   ! func(xnew) <= func(x): replace xval with xnew and one of the boundaries with x
          if(xnew.ge.xval) then
             xlowl = xval        ! Replace xlow with xval
          else
             xhighl = xval       ! Replace xhigh with xval
          end if
          
          vv = ww
          fv = fw
          ww = xval
          fw = fxval
          xval = xnew            ! Replace xval with the new point xnew
          fxval = fxnew
          
       else                      ! func(xnew) > func(x): keep x, and replace one of the boundaries with xnew
          
          if(xnew.lt.xval) then
             xlowl = xnew   ! Replace xlow with xnew
          else               
             xhighl = xnew  ! Replace xhigh with xnew
          end if
          
          if(fxnew.le.fw .or. deq(ww,xval)) then
             vv = ww
             fv = fw
             ww = xnew
             fw = fxnew
          else if(fxnew.le.fv .or. deq(vv,xval) .or. deq(vv,ww)) then
             vv = xnew
             fv = fxnew
          end if
          
       end if
       
    end do
    
    
    if(verbosityl.gt.0) call error(trim(myname)//': maximum number of iterations exceeded')
    if(present(status)) status = 9
    
    ! Return the best we have anyway:
    xmin = xval
    minimum_solver = fxval
    
  end function minimum_solver
  !*********************************************************************************************************************************
  
  
  
end module SUFR_solvers
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Do a golden-section step for minimum_solver(): find the point that is a fraction 0.382 into the larger of two intervals
!!
!! \param  x1  
!! \param  x2  
!! \param  a1  
!! \param  a2
!! 
!! \param  y1   (output)
!! \param  y2   (output)


pure subroutine golden_section(x1,x2, a1,a2, y1,y2)
  use SUFR_kinds, only: double, dbl
  
  implicit none
  real(double), intent(in)  :: x1,x2, a1,a2
  real(double), intent(out) :: y1,y2
  real(double), parameter :: golden_ratio = 0.3819660_dbl         ! The golden ratio
  
  if(x1.ge.x2) then
     y2 = a1 - x1
  else
     y2 = a2 - x1
  end if
  y1 = y2 * golden_ratio
  
end subroutine golden_section
!***********************************************************************************************************************************



