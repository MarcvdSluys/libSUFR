!> \file sorting.f90  Procedures to sort arrays


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
!> \brief  Procedures for sorting

module SUFR_sorting
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Return a list of indices index_list that sorts the members of array to ascending value.
  !!
  !! \param  array        Array of size n with values that must be sorted - use dble(array) for other variable types
  !!
  !! \retval index_list   List with indices of array values, sorted to ascending value, same dimension and size as array.
  !!                      array(index_list) gives the sorted array.
  !! 
  !! \param  mask         Mask to apply to array. If present, index_list will have zeroes after the last meaningful entry (optional)
  !! \retval index_n      Number of meaningful elements in index_list, after applying mask (optional)
  !!
  !! \note  This routine does not need to be called directly, but is implicitly called by sort_array().
  !! \see   Numerical Recipes in Fortran 77, Sect.8.4.
  
  subroutine sorted_index_list(array, index_list, mask, index_n)
    use SUFR_kinds, only: double
    use SUFR_system, only: quit_program_error
    
    implicit none
    real(double), intent(in) :: array(:)
    integer, intent(out) :: index_list(:)
    logical, intent(in), optional :: mask(:)
    integer, intent(out),optional :: index_n
    
    integer, parameter :: mm=7, nstack=50
    integer :: ii,index_i,ir,itemp,istack(nstack), jj,jstack, kk,ll, loc_list(size(index_list)), loc_index_n
    real(double) :: arr_i
    logical :: loc_mask(size(array))
    
    
    if(size(array).ne.size(index_list)) &
         call quit_program_error('libSUFR sorted_index_list():  array and index_list must have the same size',0)
    
    loc_mask = .true.
    if(present(mask)) loc_mask = mask
    
    index_list = 0
    loc_list = 0
    do jj=1,size(array)
       loc_list(jj) = jj
    end do
    
    
    ! Starting values:
    jstack = 0
    ll = 1
    ir = size(array)
    
    mainloop: do
       if(ir-ll.lt.mm) then
          
          do jj=ll+1,ir
             index_i = loc_list(jj)
             arr_i = array(index_i)
             subloop1: do ii=jj-1,ll,-1
                if(array(loc_list(ii)).le.arr_i) exit subloop1
                loc_list(ii+1) = loc_list(ii)
             end do subloop1
             if(array(loc_list(ii)).gt.arr_i) ii = ll-1
             
             loc_list(ii+1) = index_i
          end do
          
          
          !*************************************************
          ! Done - apply mask and return to caller routine
          if(jstack.eq.0) then
             
             loc_index_n = 0
             do ii=1,size(array)
                if(loc_mask(loc_list(ii))) then
                   loc_index_n = loc_index_n + 1
                   index_list(loc_index_n) = loc_list(ii)
                   if(present(index_n)) index_n = loc_index_n
                end if
             end do
             
             return
             
          end if
          !*************************************************
          
          
          ir = istack(jstack)
          ll = istack(jstack-1)
          jstack = jstack - 2
          
       else
          
          kk = (ll+ir)/2
          itemp = loc_list(kk)
          loc_list(kk) = loc_list(ll+1)
          loc_list(ll+1) = itemp
          
          if(array(loc_list(ll)).gt.array(loc_list(ir))) then
             itemp = loc_list(ll)
             loc_list(ll) = loc_list(ir)
             loc_list(ir) = itemp
          end if
          
          if(array(loc_list(ll+1)).gt.array(loc_list(ir))) then
             itemp = loc_list(ll+1)
             loc_list(ll+1) = loc_list(ir)
             loc_list(ir) = itemp
          end if
          
          if(array(loc_list(ll)).gt.array(loc_list(ll+1))) then
             itemp = loc_list(ll)
             loc_list(ll) = loc_list(ll+1)
             loc_list(ll+1) = itemp
          end if
          
          ii = ll+1
          jj = ir
          index_i = loc_list(ll+1)
          arr_i = array(index_i)
          
          
          subloop2: do
             ii = ii+1
             if(array(loc_list(ii)).lt.arr_i) cycle subloop2
             
             
             subloop3: do
                jj = jj-1
                if(array(loc_list(jj)).le.arr_i) exit
             end do subloop3
             
             
             if(jj.ge.ii) then
                itemp = loc_list(ii)
                loc_list(ii) = loc_list(jj)
                loc_list(jj) = itemp
                cycle subloop2
             end if
             
             exit subloop2
             
          end do subloop2
          
          
          loc_list(ll+1) = loc_list(jj)
          loc_list(jj) = index_i
          jstack = jstack + 2
          
          if(jstack.gt.nstack) write(0,'(A)')' sorted_index_list():  nstack is too small'
          
          if(ir-ii+1.ge.jj-ll) then
             istack(jstack) = ir
             istack(jstack-1) = ii
             ir = jj-1
          else
             istack(jstack) = jj-1
             istack(jstack-1) = ll
             ll = ii
          end if
          
       end if
       
    end do mainloop
    
  end subroutine sorted_index_list
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Sort an array to ascending value.
  !!
  !! \param array  Input: array to be sorted, output: sorted array (double)
  !!
  !! \note Uses sorted_index_list()
  
  subroutine sort_array(array)
    use SUFR_kinds, only: double
    
    implicit none
    real(double), intent(inout) :: array(:)
    
    real(double) :: array1(size(array))
    integer :: index_list(size(array))
    
    array1 = array
    call sorted_index_list(array1,index_list)
    array = array1(index_list)
    
  end subroutine sort_array
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Sort an array to ascending value - single-precision wrapper for sort_array
  !!
  !! \param array  Input: array to be sorted, output: sorted array (double)
  !! 
  !! \todo  Check whether array = array(index_list) works
  
  subroutine sort_array_sp(array)
    implicit none
    real, intent(inout) :: array(:)
    
    real :: array1(size(array))
    integer :: index_list(size(array))
    
    call sorted_index_list(dble(array), index_list)
    array1 = array
    array = array1(index_list)  ! CHECK: would array = array(index_list) work?
    
  end subroutine sort_array_sp
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Sort strings alphabetically
  !!
  !! \param  narr        Number of strings in array str
  !! \param  strlen      Length of the strings in array str
  !! \param  strarr      Array of n strings with length len
  !! \retval index_list  Sorting index
  
  subroutine sort_string_array(narr, strlen, strarr, index_list)
    implicit none
    integer, intent(in) :: narr, strlen
    character, intent(in) :: strarr(narr)*(strlen)
    integer, intent(out) :: index_list(narr)
    
    integer :: i,l,ic
    real :: score(narr)
    
    do i=1,narr
       score(i) = 0.
       do l=1,strlen
          ic = ichar(strarr(i)(l:l))
          if(ic.ge.97.and.ic.le.122) ic = ic-32
          ic = ic-32                                       ! Limits values between 1 and 64
          score(i) = score(i) + real(ic) * 64.**(strlen-l)
       end do
    end do
    
    call sorted_index_list(dble(score), index_list)
    
  end subroutine sort_string_array
  !*********************************************************************************************************************************
  
  
  
  
  
  
end module SUFR_sorting
!***********************************************************************************************************************************
