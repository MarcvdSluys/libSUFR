!> \file fitting.f90  Procedures to fit functions to data


!  Copyright 2002-2013 Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to fit functions to data

module SUFR_fitting
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief  Fit the model coefficients using a chi^2 method
  !!
  !! Do a chi^2-minimisation fit of ndat data points xdat and ydat, the latter with errors yerr, to function func.  The fit uses
  !! the ncoef coefficients stored in coef() that should be varied if the corresponding entry in varc =1, fixed if =0.
  !!
  !! The fitting function must be provided as the subroutine myFunc which returns a value y = myFunc(x, basefunc, ncoef), where
  !!   y is a linear combination of x, i.e.  y = SUM(i=1,ncoef) coef(i) * basefunc(i)(x).
  !!   For example, for a polynomial, basefunc(i) = x**(i-1).
  !!
  !! The routine returns the fit coefficients in coef(), the variance-covariance matrix in covar sith size ncov, and the chi^2 value
  !!   in chi2.
  !!
  !!
  !! \param  xdat    X values of the data
  !! \param  ydat    Y values of the data
  !! \param  yerr    Standard deviations of the y values
  !! \param  ndat    Number of data points
  !!
  !! \retval coef    Coefficients of the fitting function
  !! \param  varc    Fix coefficient coef(i) if varc(i)=0, otherwise fit it
  !! \param  ncoef   Number of coefficients of the fitting function
  !!
  !! \retval covar   Covariance matrix
  !! \param  ncov    Size of both dimensions of covar()
  !! \retval chi2    Chi^2
  !!
  !! \param  myFunc  External function that describes the model value of Y for given value X
  !!
  !! \note
  !! - Needs lfit_sort_var_covar() and solve_linear_equations_Gauss_Jordan()
  !!
  !! \see Numerical Recipes in Fortran 77, Sect.15.4
  !!
  
  subroutine linear_fit_yerr(xdat,ydat,yerr,ndat,coef,varc,ncoef,covar,ncov,chi2, myFunc)
    use SUFR_kinds, only: double
    use SUFR_system, only: warn, quit_program_error
    implicit none
    integer, parameter :: Mmax=50
    integer, intent(in) :: ncoef,varc(ncoef),ncov,ndat
    real(double), intent(in) :: xdat(ndat),ydat(ndat), yerr(ndat)
    real(double), intent(inout) :: coef(ncoef)
    real(double), intent(out) :: chi2,covar(ncov,ncov)
    
    integer :: ii,ij,ik,il,im, nfit
    real(double) :: sig2i,tot,wt,ym, basefunc(Mmax),beta(Mmax)
    external myFunc
    
    if(minval(abs(yerr)).eq.0.d0) call quit_program_error('linear_fit_yerr(): errors cannot be zero', 0)
    
    ! Determine number of parameters that need to be fit:
    nfit = 0
    do ij=1,ncoef
       if(varc(ij).ne.0) nfit = nfit+1
    end do
    if(nfit.eq.0) call warn('No parameters to be fit for', 0)
    
    covar(1:nfit,1:nfit) = 0.d0
    beta(1:nfit) = 0.d0
    
    do ii=1,ndat
       call myFunc(xdat(ii),ncoef, basefunc)
       ym = ydat(ii)
       if(nfit.lt.ncoef) then
          do ij=1,ncoef
             if(varc(ij).eq.0) ym = ym-coef(ij)*basefunc(ij)
          end do
       end if
       sig2i = 1.d0/yerr(ii)**2
       
       ij = 0
       do il=1,ncoef
          if(varc(il).ne.0) then
             ij = ij+1
             wt = basefunc(il)*sig2i
             
             ik = 0
             do im=1,il
                if(varc(im).ne.0) then
                   ik = ik+1
                   covar(ij,ik) = covar(ij,ik)+wt*basefunc(im)
                end if
             end do
             beta(ij) = beta(ij)+ym*wt
          end if
       end do
    end do
    
    do ij=2,nfit
       do ik=1,ij-1
          covar(ik,ij) = covar(ij,ik)
       end do
    end do
    
    call solve_linear_equations_Gauss_Jordan(covar,nfit,ncov, beta,1,1)
    
    ij = 0
    do il=1,ncoef
       if(varc(il).ne.0) then
          ij = ij+1
          coef(il) = beta(ij)
       end if
    end do
    
    chi2 = 0.d0
    do ii=1,ndat
       call myFunc(xdat(ii),ncoef, basefunc)
       
       tot = sum(coef(1:ncoef)*basefunc(1:ncoef))
       chi2 = chi2  +  ( (ydat(ii)-tot) / yerr(ii) )**2
    end do
    
    ! Sort the resulting variance-covariance matrix:
    call lfit_sort_var_covar(covar,ncov,ncoef,varc,nfit)
    
  end subroutine linear_fit_yerr
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Sort covariance matrix returned by linear_fit_yerr() to true order of fitting coefficients
  !!
  !! \param  covar  Covariance matrix
  !! \param  ncov   Size of both dimensions of covar()
  !! \param  ncoef  Number of coefficients of the fitting function
  !! \param  varc   Fix coefficient coef(i) if varc(i)=0, otherwise fit for it
  !! \param  nfit   Number of parameters that need to be fit for
  !!
  !! \see Numerical Recipes in Fortran 77, Sect.15.4
  !!
  
  subroutine lfit_sort_var_covar(covar,ncov,ncoef,varc,nfit)
    use SUFR_kinds, only: double
    use SUFR_system, only: swapdbl
    
    implicit none
    integer, intent(in) :: ncov,ncoef, varc(ncoef),nfit
    real(double), intent(inout) :: covar(ncov,ncov)
    
    integer :: ii,ij,ik
    
    do ii=nfit+1,ncoef
       do ij=1,ii
          covar(ii,ij) = 0.d0
          covar(ij,ii) = 0.d0
       end do
    end do
    
    ik = nfit
    
    do ij=ncoef,1,-1
       if(varc(ij).ne.0) then
          do ii=1,ncoef
             call swapdbl(covar(ii,ij), covar(ii,ik))  ! Swap the two numbers
          end do
          
          do ii=1,ncoef
             call swapdbl(covar(ij,ii), covar(ik,ii))  ! Swap the two numbers
          end do
          
          ik = ik-1
       end if
    end do
    
  end subroutine lfit_sort_var_covar
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Solve a set of linear algebraic equations using Gauss-Jordan elimination.
  !!
  !! \param matArr   Input: array containing the matrix to be inverted - output: array containing the inverted matrix  (I/O)
  !! \param Nmat     Dimension of the equation set
  !! \param NmatArr  Physical dimensions of the array matArr  (NmatArr x NmatArr)
  !!
  !! \param vecArr   Input: array containing Nvec right-hand side vectors - output: solution vectors  (I/O)
  !! \param Nvec     Number of vectors in vecArr
  !! \param NvecArr  One of the physical dimensions of the array vecArr  (NmatArr x NvecArr)
  !!
  !! \see Numerical Recipes in Fortran 77, Sect.2.1
  !!
  
  subroutine solve_linear_equations_Gauss_Jordan(matArr,Nmat,NmatArr, vecArr,Nvec,NvecArr)
    use SUFR_kinds, only: double
    use SUFR_system, only: warn, swapdbl
    
    implicit none
    integer, intent(in) :: Nmat,NmatArr, Nvec,NvecArr
    real(double), intent(inout) :: matArr(NmatArr,NmatArr),vecArr(NmatArr,NvecArr)
    
    integer, parameter :: Nmax=50  ! Maximum allowed value for Nmat
    integer :: ii,icol,irow,ij,ik,il,ll,indxc(Nmax),indxr(Nmax),ipiv(Nmax)
    real(double) :: arrmax,tmpdbl,pivinv
    
    
    icol = 0
    irow = 0
    ipiv(1:Nmat) = 0
    
    
    do ii=1,Nmat
       ! Find maximum array value:
       arrmax = 0.d0
       do ij=1,Nmat
          if(ipiv(ij).ne.1) then
             do ik=1,Nmat
                if(ipiv(ik).eq.0) then
                   if(abs(matArr(ij,ik)).ge.arrmax) then
                      arrmax = abs(matArr(ij,ik))
                      irow = ij
                      icol = ik
                   end if
                else if(ipiv(ik).gt.1) then
                   call warn('Singular matrix in solve_linear_equations_Gauss_Jordan()',0)
                end if
             end do
          end if
       end do
       
       ipiv(icol) = ipiv(icol) + 1
       
       if(irow.ne.icol) then
          do il=1,Nmat
             call swapdbl(matArr(irow,il), matArr(icol,il))
          end do
          
          do il=1,Nvec
             call swapdbl(vecArr(irow,il), vecArr(icol,il))
          end do
       end if
       
       indxr(ii) = irow
       indxc(ii) = icol
       
       if(matArr(icol,icol).eq.0.d0) call warn('Singular matrix in solve_linear_equations_Gauss_Jordan()', 0)
       
       pivinv = 1.d0/matArr(icol,icol)
       matArr(icol,icol) = 1.d0
       
       matArr(icol,1:Nmat) = matArr(icol,1:Nmat)*pivinv
       vecArr(icol,1:Nvec) = vecArr(icol,1:Nvec)*pivinv
       
       do ll=1,Nmat
          if(ll.ne.icol) then
             tmpdbl = matArr(ll,icol)
             matArr(ll,icol) = 0.d0
             matArr(ll,1:Nmat) = matArr(ll,1:Nmat) - matArr(icol,1:Nmat)*tmpdbl
             vecArr(ll,1:Nvec) = vecArr(ll,1:Nvec) - vecArr(icol,1:Nvec)*tmpdbl
          end if
       end do
    end do
    
    do il=Nmat,1,-1
       if(indxr(il).ne.indxc(il)) then
          do ik=1,Nmat
             call swapdbl(matArr(ik,indxr(il)), matArr(ik,indxc(il)))
          end do
       end if
    end do
    
  end subroutine solve_linear_equations_Gauss_Jordan
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  User-provided base function to fit a polynomial to
  !!
  !! \param  xval      X value to evaluate yval = basefunc_polynimial(xval) for
  !! \param  ncoef     Number of coefficients for the base function
  !!
  !! \retval basefunc  Base function i for coefficent i.  For a polynomial, BF(i) = x**(i-1)
  
  subroutine basefunc_polynomial(xval,ncoef, basefunc)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in) :: ncoef
    real(double), intent(in) :: xval
    real(double), intent(out) :: basefunc(ncoef)
    
    integer :: coefi
    
    basefunc(1) = 1.d0
    do coefi = 2,ncoef
       basefunc(coefi) = basefunc(coefi-1) * xval
    end do
    
  end subroutine basefunc_polynomial
  !*********************************************************************************************************************************
  
  
  
  
end module SUFR_fitting
!***********************************************************************************************************************************

