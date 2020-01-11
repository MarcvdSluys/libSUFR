!> \file solvers.f90  Procedures to solve equations

   
!  Copyright (c) 2002-2020  Marc van der Sluys - marc.vandersluys.nl
!   
!  This file is part of the libSUFR package, 
!  see: http://libsufr.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.




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
  !> \brief  Use Brent's method and parabolic interpolation to find the minimum of function func that lies between xa and xc.
  !!
  !! \param func      The function of which the minimum is to be found
  !! \param ax        The lower limit for the x-value of the minimum:  xa < x_min < xc
  !! \param bx        A good guess for the x-value of the minimum:  xa < xb < xc  and  func(xb) < min(funx(xa),func(xc))
  !! \param cx        The upper limit for the x-value of the minimum:  xa < x_min < xc
  !! \param accur     Relative accuracy with which the minimum is to be found
  !! 
  !! \param xmin      X-value of the minimum (output)
  !! \param status    Status: 0-ok, 1-maximum number of iterations exceeded   (output, optional)
  !! \param verbose   Verbosity: 0-print nothing, 1-print errors, 2-print warnings, 3-print info
  !!                  (output, optional argument, default=2)
  !! 
  !! \retval minimum_solver  The value of the minimum of the function func, to accuracy accur
  !!
  !! \see Numerical Recipes in Fortran 77, Sect.10.2.
  
  function minimum_solver(func,ax,bx,cx,accur,xmin, status,verbose)
    use SUFR_kinds, only: double, dbl
    use SUFR_numerics, only: deq
    
    implicit none
    real(double), intent(in) :: ax,bx,cx,accur
    real(double), intent(out) :: xmin
    integer, intent(out), optional :: status
    integer, intent(in), optional :: verbose
    real(double) :: minimum_solver
    real(double), external :: func
    
    integer, parameter :: max_iter = 100
    real(double), parameter :: eps = epsilon(0.0_dbl)  ! Machine precision
    
    integer :: iter,verbosity
    real(double) :: a,b,d,e,e1, p,q,r, accur1,accur2
    real(double) :: u,v,w,x,xm, fu,fv,fw,fx
    
    
    if(present(status)) status = 0
    verbosity = 2
    if(present(verbose)) verbosity = verbose
    
    a = min(ax,cx)
    b = max(ax,cx)
    
    x = bx
    v = bx
    w = bx
    
    fx = func(x)
    fv = fx
    fw = fx
    
    ! Prevent compiler complaints:
    d = 1.0_dbl
    e = 2.0_dbl
    
    
    accur1 = accur + eps                                                    ! Use absolute accuracy
    accur2 = 2*accur1
    
    do iter=1,max_iter
       xm = 0.5_dbl*(a+b)
       
       if( abs(x-xm) .le. accur2 - 0.5_dbl*(b-a) ) then                     ! Then we have a sufficiently accurate solution
          xmin = x
          minimum_solver = fx
          return
       end if
       
       
       if(abs(e).gt.accur1) then
          r = (x-w) * (fx-fv)
          q = (x-v) * (fx-fw)
          p = (x-v)*q - (x-w)*r
          q = 2*(q-r)
          if(q.gt.0.0_dbl) p = -p
          q = abs(q)
          e1 = e
          e = d
          
          if(abs(p).ge.abs(0.5_dbl*q*e1) .or. p.le.q*(a-x) .or. p.ge.q*(b-x)) then
             call golden_section(x,xm,a,b,d,e)                                         ! Do a golden-section step
          else
             d = p/q
             u = x+d
             if(u-a.lt.accur2 .or. b-u.lt.accur2) d = sign(accur1,xm-x)
          end if
       else
          call golden_section(x,xm,a,b,d,e)                                            ! Do a golden-section step
       end if
       
       ! Determine the point u where func must be evaluated next:
       if(abs(d).ge.accur1) then
          u = x + d
       else
          u = x + sign(accur1,d)
       end if
       
       
       fu = func(u)
       
       if(fu.le.fx) then   ! func(u) <= func(x): replace x with u and one of the boundaries with x
          if(u.ge.x) then  ! Replace a with x
             a = x
          else
             b = x         ! Replace b with x
          end if
          
          v = w
          fv = fw
          w = x
          fw = fx
          x = u            ! Replace x with the new point u
          fx = fu
          
       else                ! func(u) > func(x): keep x, and replace one of the boundaries with u
          
          if(u.lt.x) then  ! Replace a with u
             a = u
          else             ! Replace b with u
             b = u
          end if
          
          if(fu.le.fw .or. deq(w,x)) then
             v = w
             fv = fw
             w = u
             fw = fu
          else if(fu.le.fv .or. deq(v,x) .or. deq(v,w)) then
             v = u
             fv = fu
          end if
          
       end if
       
    end do
    
    
    if(verbosity.gt.0) write(0,'(A)') ' libSUFR - minimum_solver():  maximum number of iterations exceeded'
    if(present(status)) status = 1
    
    ! Return the best we have anyway:
    xmin = x
    minimum_solver = fx
    
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



