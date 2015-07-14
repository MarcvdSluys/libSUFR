!> \file fitting.f90  Procedures to fit functions to data


!  Copyright (c) 2002-2015  Marc van der Sluys - marc.vandersluys.nl
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
  !! Do a chi^2-minimisation fit of ndat data points xdat and ydat, the latter with errors yErr, to function func.  The fit uses
  !! the nCoef coefficients stored in fCoef() that should be varied if the corresponding entry in varc =1, fixed if =0.
  !!
  !! The fitting function must be provided as the subroutine myFunc which returns a value y = myFunc(x, basefunc, nCoef), where
  !!   y is a linear combination of x, i.e.  y = SUM(i=1,nCoef) fCoef(i) * basefunc(i)(x).
  !!   For example, for a polynomial, basefunc(i) = x**(i-1).
  !!
  !! The routine returns the fit coefficients in fCoef(), the variance-covariance matrix in covMat sith size nCov, and the chi^2
  !!   value in chiSq.
  !!
  !!
  !! \param  nDat      Number of data points
  !! \param  xDat      X values of the data (nDat)
  !! \param  yDat      Y values of the data (nDat)
  !! \param  yErr      Errors (standard deviations) of the y values (nDat)
  !!
  !! \param  nCoef     Number of coefficients of the fitting function
  !! \retval fCoef     Coefficients of the fitting function (nCoef)
  !! \param  freeCoef  Fix coefficient coef(i) if varc(i)=0, otherwise let if vary freely and fit it (nCoef)
  !!
  !! \param  nCov      Size of both dimensions of covMat()
  !! \retval covMat    Covariance matrix (nCov,nCov)
  !!
  !! \retval chiSq     Chi^2 - chi squared
  !!
  !! \param  myFunc    External subroutine that describes the model value of Y for given value X
  !!
  !! \note
  !! - Needs sort_var_covar() and solve_linear_equations_Gauss_Jordan()
  !!
  !! \see Numerical Recipes in Fortran 77, Sect.15.4
  !!
  
 subroutine linear_fit_yerr(nDat, xDat,yDat, yErr,  nCoef,fCoef,freeCoef,  nCov,covMat, chiSq, myFunc)
    use SUFR_kinds, only: double
    use SUFR_system, only: warn, quit_program_error
    implicit none
    integer, parameter :: mMax = 50
    integer, intent(in) :: nDat, nCoef,freeCoef(nCoef), nCov
    real(double), intent(in) :: xDat(nDat),yDat(nDat), yErr(nDat)
    real(double), intent(inout) :: fCoef(nCoef)
    real(double), intent(out) :: chiSq, covMat(nCov,nCov)
    
    integer :: ii,ij,ik,il,im, nfit
    real(double) :: err2i,tot,wt,ym, basefunc(mMax),beta(mMax)
    external myFunc
    
    if(minval(abs(yErr)).lt.10*tiny(yErr)) call quit_program_error('libSUFR linear_fit_yerr(): errors cannot be zero', 0)
    
    ! Determine number of parameters that need to be fit:
    nfit = 0
    do ij=1,nCoef
       if(freeCoef(ij).ne.0) nfit = nfit+1
    end do
    if(nfit.eq.0) call warn('libSUFR linear_fit_yerr():  No parameters to be fit for', 0)
    
    covMat(1:nfit,1:nfit) = 0.d0
    beta(1:nfit) = 0.d0
    
    do ii=1,nDat
       call myFunc(xDat(ii),nCoef, basefunc)
       ym = yDat(ii)
       if(nfit.lt.nCoef) then
          do ij=1,nCoef
             if(freeCoef(ij).eq.0) ym = ym - fCoef(ij)*basefunc(ij)
          end do
       end if
       err2i = 1.d0/yErr(ii)**2  ! 1/sigma_i^2
       
       ij = 0
       do il=1,nCoef
          if(freeCoef(il).ne.0) then
             ij = ij + 1
             wt = basefunc(il)*err2i
             
             ik = 0
             do im=1,il
                if(freeCoef(im).ne.0) then
                   ik = ik + 1
                   covMat(ij,ik) = covMat(ij,ik)+wt*basefunc(im)
                end if
             end do
             beta(ij) = beta(ij)+ym*wt
          end if
       end do
    end do
    
    do ij=2,nfit
       do ik=1,ij-1
          covMat(ik,ij) = covMat(ij,ik)
       end do
    end do
    
    ! Solve the linear algebraic equations using Gauss-Jordan elimination:
    call solve_linear_equations_Gauss_Jordan(covMat,nfit,nCov, beta,1,1)
    
    ij = 0
    do il=1,nCoef
       if(freeCoef(il).ne.0) then
          ij = ij+1
          fCoef(il) = beta(ij)
       end if
    end do
    
    chiSq = 0.d0
    do ii=1,nDat
       call myFunc(xDat(ii),nCoef, basefunc)
       
       tot = sum(fCoef(1:nCoef)*basefunc(1:nCoef))
       chiSq = chiSq  +  ( (yDat(ii)-tot) / yErr(ii) )**2
    end do
    
    ! Sort the resulting variance-covariance matrix:
    call sort_var_covar(covMat,nCov,nCoef,freeCoef,nfit)
    
  end subroutine linear_fit_yerr
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Levenberg-Marquardt method to reduce the value of chi-squared of a fit of a set of data points with errors in Y to a
  !!         non-linear function
  !!
  !! You will need to call this routine repeatedly, with different values for iterStat, until chiSq no longer decreases
  !!   (significantly)
  !!
  !! \param nDat      Number of data points in X and Y
  !! \param xDat      X data points to fit
  !! \param yDat      Y data points to fit
  !! \param yErr      Errors (standard deviations) for yDat
  !!
  !! \param nCoef     Number of coefficients used to describe the non-linear function myFunc
  !! \param fCoef     Coefficients for the non-linear function myFunc, updated after each call
  !! \param freeCoef  Determines which coefficients for the non-linear function myFunc should be fitted for.  Set freeCoef(i) = 0 
  !!                    in order to keep fCoef(i) fixed
  !!
  !! \param nMat      Size of the matrices; > nCoef
  !! \param covMat    Variance-covariance matrix - returned on last call with iterStat.  Fixed parameters return zero (co)variances
  !! \param curvMat   Hessian/curvature matrix - double partial derivative of chi squared to two coefficients fCoef
  !!
  !! \param chiSq     Chi squared
  !! \param iterStat  Iteration status;  Set to <0 for initialisation in the first call.  Set to 0 to return the 
  !!                    variance-covariance and curvature matrices on the last call.  IterStat decreases 10x if chiSq becomes 
  !!                    smaller, and increases 10x otherwise.
  !! \param myFunc    External subroutine that describes the model value of Y and partial derivatives dY/dXi for given value X and
  !!                    function coefficients fCoef
  !!
  !!
  !! \see  Numerical Recipes in Fortran, 15.5: Modelling of Data / Non-linear models
  !! 
  !! \note  Uses sort_var_covar(), solve_linear_equations_Gauss_Jordan() and nonlin_fit_eval()
  
  subroutine nonlin_fit_yerr(nDat, xDat,yDat, yErr,  nCoef,fCoef,freeCoef,  nMat,covMat,curvMat,  chiSq, iterStat, myFunc)
    use SUFR_kinds, only: double
    use SUFR_numerics, only: deq0
    
    implicit none
    integer, intent(in) :: nDat, nCoef,freeCoef(nCoef), nMat
    real(double), intent(in) :: yErr(nDat), xDat(nDat), yDat(nDat)
    real(double), intent(inout) :: fCoef(nCoef), iterStat
    real(double), intent(out) :: curvMat(nMat,nMat), covMat(nMat,nMat), chiSq
    
    integer, parameter :: mMax = 20
    integer :: iFit,mFit, iCoef,cntCoef
    real(double) :: oChiSq,tryCoef(mMax),dChiSq(mMax),dfCoef(mMax)
    
    save oChiSq,tryCoef,dChiSq,dfCoef,mFit
    external myFunc
    
    
    ! An EXTERNAL subroutine must be CALLed - use a dummy call here, since the subroutine name is only passed on:
    if(.false.) call myFunc()
    
    
    ! Initialisation on the first call:
    if(iterStat.lt.0.d0) then
       mFit = 0
       do iCoef=1,nCoef
          if(freeCoef(iCoef).ne.0) mFit = mFit + 1
       end do
       
       iterStat = 0.001d0
       
       ! Evaluate the linearized curvature matrix and vector dChiSq/dfCoef, and compute chi squared:
       call nonlin_fit_eval(xDat,yDat, yErr, nDat, fCoef,freeCoef,nCoef, curvMat,dChiSq, nMat, chiSq, myFunc)
       
       oChiSq = chiSq
       tryCoef(1:nCoef) = fCoef(1:nCoef)
    end if
    
    
    ! Augment the diagonal elements of the variance-covariance matrix:
    covMat(1:mFit,1:mFit) = curvMat(1:mFit,1:mFit)
    dfCoef(1:mFit)       = dChiSq(1:mFit)
    
    do iFit=1,mFit
       covMat(iFit,iFit) = curvMat(iFit,iFit) * (1.d0+iterStat)
    end do
    
    
    ! Solve the linear algebraic equations using Gauss-Jordan elimination:
    call solve_linear_equations_Gauss_Jordan(covMat,mFit,nMat,dfCoef,1,1)
    
    
    ! Compute the variance-covariance and curvature matrices on the last call, and return:
    if(deq0(iterStat)) then
       call sort_var_covar(covMat,  nMat,  nCoef, freeCoef,  mFit)
       call sort_var_covar(curvMat, nMat,  nCoef, freeCoef,  mFit)
       return
    end if
    
    
    ! Update the trial coefficients:
    cntCoef = 0
    do iCoef=1,nCoef
       if(freeCoef(iCoef).ne.0) then
          cntCoef = cntCoef+1
          tryCoef(iCoef) = fCoef(iCoef) + dfCoef(cntCoef)
       end if
    end do
    
    
    ! Evaluate the linearized curvature matrix and vector dChiSq/dfCoef, and compute chi squared:
    call nonlin_fit_eval(xDat,yDat, yErr, nDat, tryCoef,freeCoef,nCoef, covMat, dfCoef,nMat, chiSq, myFunc)
    
    
    ! If the new chi squared is better (smaller) than the old value, accept this solution and decrease iterStat...
    if(chiSq.lt.oChiSq) then
       iterStat = 0.1d0*iterStat
       oChiSq = chiSq
       
       curvMat(1:mFit,1:mFit) = covMat(1:mFit,1:mFit)
       dChiSq(1:mFit) = dfCoef(1:mFit)
       
       fCoef(1:nCoef) = tryCoef(1:nCoef)
       
    else                                        !  ...otherwise keep the old chi squared, and, if larger, increase iterStat:
       
       if(chiSq.gt.oChiSq) iterStat = 10*iterStat
       chiSq = oChiSq
       
    end if
    
  end subroutine nonlin_fit_yerr
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Evaluate the linearized fitting matrix curvMat and vector dChiSq, and calculate the chi squared (chiSq)
  !!
  !! \param xDat     X data points to fit
  !! \param yDat     Y data points to fit
  !! \param yErr     Errors (standard deviations) for yDat
  !! \param nDat     Number of data points in X and Y
  !!
  !! \param fCoef    Coefficients for the non-linear function myFunc, updated after each call
  !! \param freeCoef Determines which coefficients for the non-linear function myFunc should be fitted for.  Set freeCoef(i) = 0 in 
  !!                   order to keep fCoef(i) fixed
  !! \param nCoef    Number of coefficients used to describe the non-linear function myFunc
  !!
  !! \param curvMat  Hessian/curvature matrix - double partial derivative of chi squared to two coefficients fCoef
  !! \param dChiSq   Vector containing the partial derivatives of chiSq to each coefficient in fCoef
  !! \param nMat     Size of the matrices; > nCoef
  !!
  !! \param chiSq    Chi squared
  !! \param myFunc   External subroutine that describes the model value of Y and partial derivatives dY/dXi for given value X and
  !!                   function coefficients fCoef
  !!
  !!
  !! \see  Numerical Recipes in Fortran, 15.5: Modelling of Data / Non-linear models
  
  subroutine nonlin_fit_eval(xDat,yDat, yErr, nDat, fCoef,freeCoef,nCoef, curvMat,dChiSq, nMat, chiSq, myFunc)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in) :: nCoef,nMat,nDat,freeCoef(nCoef)
    real(double), intent(in) :: xDat(nDat),yDat(nDat),yErr(nDat), fCoef(nCoef)
    real(double), intent(out) :: chiSq,curvMat(nMat,nMat),dChiSq(nCoef)
    
    integer, parameter :: mMax = 20
    integer :: iFit,jFit,mFit, iDat, iCoef,jCoef, cntCoefi,cntCoefj
    real(double) :: dy,err2i,wt,yMod,dyda(mMax)
    external myFunc
    
    
    ! Determine the number of non-fixed parameters:
    mFit = 0
    do iCoef=1,nCoef
       if(freeCoef(iCoef).ne.0) mFit = mFit + 1
    end do
    
    
    ! Initialise the curvature matrix and chiSq-derivative vector:
    curvMat = 0.d0
    dChiSq = 0.d0
    
    
    ! Sum over all data points:
    chiSq = 0.d0
    do iDat=1,nDat
       call myFunc(xDat(iDat), fCoef,nCoef, yMod, dyda)
       
       err2i = 1.d0/(yErr(iDat)**2)  ! 1/sigma^2
       dy = yDat(iDat) - yMod
       
       cntCoefi = 0
       do iCoef=1,nCoef
          if(freeCoef(iCoef).ne.0) then
             cntCoefi = cntCoefi + 1
             wt = dyda(iCoef)*err2i  ! dy/dfCoef_i / sigma^2
             
             cntCoefj = 0
             do jCoef=1,iCoef
                if(freeCoef(jCoef).ne.0) then
                   cntCoefj = cntCoefj + 1
                   curvMat(cntCoefi,cntCoefj) = curvMat(cntCoefi,cntCoefj) + wt*dyda(jCoef)  ! dy/dfCoef_i dy/dfCoef_j / sigma^2
                end if
             end do  ! jCoef
             
             dChiSq(cntCoefi) = dChiSq(cntCoefi) + dy*wt
          end if
       end do  ! iCoef
       
       chiSq = chiSq + dy**2 * err2i  ! Compute chi squared
    end do  ! iDat
    
    
    ! Fill in the symmetric side of the curvature matrix:
    do iFit=2,mFit
       do jFit=1,iFit-1
          curvMat(jFit,iFit) = curvMat(iFit,jFit)
       end do  ! jFit
    end do  ! iFit
    
  end subroutine nonlin_fit_eval
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Dummy example function myFunc for nonlin_fit_yerr(): return the value and partial derivatives
  !!
  !! \param xDat   Input X values for the data points
  !! \param fCoef  Vector of coefficients that describe the function
  !! \param nCoef  Number of coefficients
  !!
  !! \retval yDat  Y values for the data points
  !! \retval dyda  Partial derivatives for yDat:  1: dy/dfCoef(1),  ...,  n: dy/dfCoef(n)
  !!
  !!
  !! \note  Write a subroutine with the same interface to use with nonlin_fit_yerr()
  
  subroutine nonlin_fit_example_myFunc(xDat, fCoef,nCoef, yDat,dyda)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(in) :: nCoef
    real(double), intent(in) :: xDat,fCoef(nCoef)
    real(double), intent(out) :: yDat,dyda(nCoef)
    
    yDat    = fCoef(1)*xDat**2 + fCoef(2)   ! Replace with desired function
    dyda(1) = fCoef(2) * xDat               ! Replace with partial derivative w.r.t. first variable - dyDat/dfCoef(1)
    ! ...
    dyda(nCoef) = fCoef(1)                  ! Replace with partial derivative w.r.t. n-th variable - dyDat/dfCoef(nCoef)
    
  end subroutine nonlin_fit_example_myFunc
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Sort covariance matrix returned by linear_fit_yerr() and nonlin_fit_yerr() to true order of fitting coefficients
  !!
  !! \param  covMat    Variance-covariance matrix
  !! \param  nCov      Size of both dimensions of covMat()
  !! \param  nCoef     Number of coefficients of the fitting function
  !! \param  freeCoef  Fix coefficient coef(i) if freeCoef(i)=0, otherwise fit for it
  !! \param  nFit      Number of parameters that need to be fit for
  !!
  !! \see Numerical Recipes in Fortran 77, Sect.15.4
  !!
  
  subroutine sort_var_covar(covMat,nCov, nCoef,freeCoef, nFit)
    use SUFR_kinds, only: double
    use SUFR_system, only: swapdbl
    
    implicit none
    integer, intent(in) :: nCov,nCoef, freeCoef(nCoef),nFit
    real(double), intent(inout) :: covMat(nCov,nCov)
    
    integer :: ii,ij,ik
    
    do ii=nFit+1,nCoef
       do ij=1,ii
          covMat(ii,ij) = 0.d0
          covMat(ij,ii) = 0.d0
       end do
    end do
    
    ik = nFit
    
    do ij=nCoef,1,-1
       if(freeCoef(ij).ne.0) then
          do ii=1,nCoef
             call swapdbl(covMat(ii,ij), covMat(ii,ik))  ! Swap the two numbers
          end do
          
          do ii=1,nCoef
             call swapdbl(covMat(ij,ii), covMat(ik,ii))  ! Swap the two numbers
          end do
          
          ik = ik-1
       end if
    end do
    
  end subroutine sort_var_covar
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Solve a set of linear algebraic equations using Gauss-Jordan elimination.
  !!
  !! \param matArr   Input: array containing the matrix to be inverted - output: array containing the inverted matrix  (I/O)
  !! \param nMat     Dimension of the equation set
  !! \param nMatArr  Physical dimensions of the array matArr  (nMatArr x nMatArr)
  !!
  !! \param vecArr   Input: array containing nVec right-hand side vectors - output: solution vectors  (I/O)
  !! \param nVec     Number of vectors in vecArr
  !! \param nVecArr  One of the physical dimensions of the array vecArr  (nMatArr x nVecArr)
  !!
  !! \see Numerical Recipes in Fortran 77, Sect.2.1
  !!
  
  subroutine solve_linear_equations_Gauss_Jordan(matArr,nMat,nMatArr, vecArr,nVec,nVecArr)
    use SUFR_kinds, only: double
    use SUFR_system, only: warn, swapdbl
    
    implicit none
    integer, intent(in) :: nMat,nMatArr, nVec,nVecArr
    real(double), intent(inout) :: matArr(nMatArr,nMatArr),vecArr(nMatArr,nVecArr)
    
    integer, parameter :: Nmax = 50  ! Maximum allowed value for nMat
    integer :: ii,icol,irow,ij,ik,il,ll,indxc(Nmax),indxr(Nmax),ipiv(Nmax)
    real(double) :: arrmax,tmpdbl,pivinv
    
    
    icol = 0
    irow = 0
    ipiv(1:nMat) = 0
    
    
    do ii=1,nMat
       ! Find maximum array value:
       arrmax = 0.d0
       do ij=1,nMat
          if(ipiv(ij).ne.1) then
             do ik=1,nMat
                if(ipiv(ik).eq.0) then
                   if(abs(matArr(ij,ik)).ge.arrmax) then
                      arrmax = abs(matArr(ij,ik))
                      irow = ij
                      icol = ik
                   end if
                else if(ipiv(ik).gt.1) then
                   call warn('libSUFR solve_linear_equations_Gauss_Jordan():  singular matrix',0)
                end if
             end do
          end if
       end do
       
       ipiv(icol) = ipiv(icol) + 1
       
       if(irow.ne.icol) then
          do il=1,nMat
             call swapdbl(matArr(irow,il), matArr(icol,il))
          end do
          
          do il=1,nVec
             call swapdbl(vecArr(irow,il), vecArr(icol,il))
          end do
       end if
       
       indxr(ii) = irow
       indxc(ii) = icol
       
       if(abs(matArr(icol,icol)).lt.10*tiny(matArr)) call warn('libSUFR solve_linear_equations_Gauss_Jordan():  singular matrix', 0)
       
       pivinv = 1.d0/matArr(icol,icol)
       matArr(icol,icol) = 1.d0
       
       matArr(icol,1:nMat) = matArr(icol,1:nMat)*pivinv
       vecArr(icol,1:nVec) = vecArr(icol,1:nVec)*pivinv
       
       do ll=1,nMat
          if(ll.ne.icol) then
             tmpdbl = matArr(ll,icol)
             matArr(ll,icol) = 0.d0
             matArr(ll,1:nMat) = matArr(ll,1:nMat) - matArr(icol,1:nMat)*tmpdbl
             vecArr(ll,1:nVec) = vecArr(ll,1:nVec) - vecArr(icol,1:nVec)*tmpdbl
          end if
       end do
    end do
    
    do il=nMat,1,-1
       if(indxr(il).ne.indxc(il)) then
          do ik=1,nMat
             call swapdbl(matArr(ik,indxr(il)), matArr(ik,indxc(il)))
          end do
       end if
    end do
    
  end subroutine solve_linear_equations_Gauss_Jordan
  !*********************************************************************************************************************************
  
  
  
  
  !*********************************************************************************************************************************
  !> \brief  User-provided base function to fit a polynomial to
  !!
  !! \param  xVal      X value to evaluate yval = basefunc_polynimial(xVal) for
  !! \param  nCoef     Number of coefficients for the base function
  !!
  !! \retval baseFunc  Base function i for coefficent i.  For a polynomial, BF(i) = x**(i-1)
  
  subroutine basefunc_polynomial(xVal,nCoef, baseFunc)
    use SUFR_kinds, only: double
    implicit none
    integer, intent(in) :: nCoef
    real(double), intent(in) :: xVal
    real(double), intent(out) :: baseFunc(nCoef)
    
    integer :: coefi
    
    baseFunc(1) = 1.d0
    do coefi = 2,nCoef
       baseFunc(coefi) = baseFunc(coefi-1) * xVal
    end do
    
  end subroutine basefunc_polynomial
  !*********************************************************************************************************************************
  
  
  
  
end module SUFR_fitting
!***********************************************************************************************************************************

