!> \file sorting.f90  Procedures to sort arrays


!  Copyright 2002-2011 Marc van der Sluys - marc.vandersluys.nl
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
  !! \param  n            Size of array (int).
  !! \param  array        Array of size n with values that must be sorted (double) - use dble(array) for other variable types
  !!
  !! \retval index_list  List with indices of array values, sorted to ascending value.  
  !!                     array(index_list) gives the sorted array (int).
  !!
  !! \note This routine does not need to be called directly, but is implicitly called by sort_array().
  !! \see Numerical Recipes in Fortran 77, Sect.8.4.
  
  subroutine sorted_index_list(n,array,index_list)
    use SUFR_kinds
    
    implicit none
    integer, intent(in) :: n
    real(double), intent(in) :: array(n)
    integer, intent(out) :: index_list(n)
    
    integer, parameter :: m=7,nstack=50
    real(double) :: a
    integer :: i,index_i,ir,itemp,j,jstack,k,l,istack(nstack)
    
    do j=1,n
       index_list(j) = j
    end do
    
    jstack = 0
    l = 1
    ir = n
    
1   if(ir-l.lt.m) then
       
       do j=l+1,ir
          index_i = index_list(j)
          a = array(index_i)
          do i=j-1,l,-1
             if(array(index_list(i)).le.a) goto 2
             index_list(i+1) = index_list(i)
          end do
          i = l-1
2         index_list(i+1) = index_i
       end do
       
       if(jstack.eq.0) return
       ir = istack(jstack)
       l = istack(jstack-1)
       jstack = jstack-2
       
    else
       
       k = (l+ir)/2
       itemp = index_list(k)
       index_list(k) = index_list(l+1)
       index_list(l+1) = itemp
       
       if(array(index_list(l)).gt.array(index_list(ir))) then
          itemp = index_list(l)
          index_list(l) = index_list(ir)
          index_list(ir) = itemp
       end if
       if(array(index_list(l+1)).gt.array(index_list(ir))) then
          itemp = index_list(l+1)
          index_list(l+1) = index_list(ir)
          index_list(ir) = itemp
       end if
       if(array(index_list(l)).gt.array(index_list(l+1))) then
          itemp = index_list(l)
          index_list(l) = index_list(l+1)
          index_list(l+1) = itemp
       end if
       
       i = l+1
       j = ir
       index_i = index_list(l+1)
       a = array(index_i)
       
3      continue
       i = i+1
       if(array(index_list(i)).lt.a) goto 3
       
4      continue
       j = j-1
       if(array(index_list(j)).gt.a) goto 4
       if(j.lt.i) goto 5
       itemp = index_list(i)
       index_list(i) = index_list(j)
       index_list(j) = itemp
       goto 3
       
5      index_list(l+1) = index_list(j)
       index_list(j) = index_i
       jstack = jstack+2
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
    
    goto 1
  end subroutine sorted_index_list
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Sort an array to ascending value.
  !!
  !! \param array  Input: array to be sorted, output: sorted array (double)
  !! \note Uses sorted_index_list()
  
  subroutine sort_array(array)
    use SUFR_kinds
    
    implicit none
    real(double), intent(inout) :: array(:)
    
    real(double) :: array1(size(array))
    integer :: index_list(size(array)), n
    
    n = size(array)
    array1 = array
    call sorted_index_list(n,array1,index_list)
    array = array1(index_list)
    
  end subroutine sort_array
  !*********************************************************************************************************************************
  
  
  
  !*********************************************************************************************************************************
  !> \brief  Sort strings alphabetically
  !!
  !! \param  n     Number of strings in array str
  !! \param  len   Length of the strings in array str
  !! \param  str   Array of n strings with length len
  !! \retval indx  Sorting index
  
  subroutine sort_string_array(n,len,str,indx)
    implicit none
    integer, intent(in) :: n, len
    character, intent(in) :: str(n)*(len)
    integer, intent(out) :: indx(n)
    
    integer :: i,l,ic
    real :: score(n)
    
    do i=1,n
       score(i) = 0.
       do l=1,len
          ic = ichar(str(i)(l:l))
          if(ic.ge.97.and.ic.le.122) ic = ic-32
          ic = ic-32                                       ! Limits values between 1 and 64
          score(i) = score(i) + real(ic) * 64.**(len-l)
       end do
    end do
    
    call sorted_index_list(n,dble(score),indx)
    
  end subroutine sort_string_array
  !*********************************************************************************************************************************
  
  
  
  
  
  
end module SUFR_sorting
!***********************************************************************************************************************************
