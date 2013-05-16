!> \file sorting.f90  Procedures to sort arrays


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
    
    integer, parameter :: m=7, nstack=50
    real(double) :: a
    integer :: i,index_i,ir,itemp,j,jstack,k,l,istack(nstack), loc_list(size(index_list))
    logical :: loc_mask(size(array))
    
    if(size(array).ne.size(index_list)) &
         call quit_program_error('sorted_index_list():  array and index_list must have the same size',0)
    
    ir = size(array)
    loc_mask = .true.
    if(present(mask)) loc_mask = mask
    
    loc_list = 0
    do j=1,ir
       loc_list(j) = j
    end do
    
    
    jstack = 0
    l = 1
    
    mainloop: do
       if(ir-l.lt.m) then
          
          do j=l+1,ir
             index_i = loc_list(j)
             a = array(index_i)
             subloop1: do i=j-1,l,-1
                if(array(loc_list(i)).le.a) exit subloop1
                loc_list(i+1) = loc_list(i)
             end do subloop1
             if(array(loc_list(i)).gt.a) i = l-1
             
             loc_list(i+1) = index_i
          end do
          
          
          !*************************************************
          ! Done - apply mask and return to caller routine
          if(jstack.eq.0) then
             index_n = 0
             do i=1,size(array)
                if(loc_mask(loc_list(i))) then
                   index_n = index_n + 1
                   index_list(index_n) = loc_list(i)
                end if
             end do
             return
          end if
          !*************************************************
          
          
          ir = istack(jstack)
          l = istack(jstack-1)
          jstack = jstack - 2
          
       else
          
          k = (l+ir)/2
          itemp = loc_list(k)
          loc_list(k) = loc_list(l+1)
          loc_list(l+1) = itemp
          
          if(array(loc_list(l)).gt.array(loc_list(ir))) then
             itemp = loc_list(l)
             loc_list(l) = loc_list(ir)
             loc_list(ir) = itemp
          end if
          
          if(array(loc_list(l+1)).gt.array(loc_list(ir))) then
             itemp = loc_list(l+1)
             loc_list(l+1) = loc_list(ir)
             loc_list(ir) = itemp
          end if
          
          if(array(loc_list(l)).gt.array(loc_list(l+1))) then
             itemp = loc_list(l)
             loc_list(l) = loc_list(l+1)
             loc_list(l+1) = itemp
          end if
          
          i = l+1
          j = ir
          index_i = loc_list(l+1)
          a = array(index_i)
          
          
          subloop2: do
             i = i+1
             if(array(loc_list(i)).lt.a) cycle subloop2
             
             
             subloop3: do
                j = j-1
                if(array(loc_list(j)).le.a) exit
             end do subloop3
             
             
             if(j.ge.i) then
                itemp = loc_list(i)
                loc_list(i) = loc_list(j)
                loc_list(j) = itemp
                cycle subloop2
             end if
             
             exit subloop2
             
          end do subloop2
          
          
          loc_list(l+1) = loc_list(j)
          loc_list(j) = index_i
          jstack = jstack + 2
          
          if(jstack.gt.nstack) write(0,'(A)')' sorted_index_list():  nstack is too small'
          
          if(ir-i+1.ge.j-l) then
             istack(jstack) = ir
             istack(jstack-1) = i
             ir = j-1
          else
             istack(jstack) = j-1
             istack(jstack-1) = l
             l = i
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
