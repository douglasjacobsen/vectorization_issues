! Define _CONTIG to add the Contiguous attribute
! Define _CONCUR to use `do concurrent` for vector loops

#define DIM1SIZE 10000
#define DIM2SIZE 100
#define NUMTESTS 3

!#define _CONTIG
!#define _CONCUR

#ifdef _CONTIG

#define ATTRS , contiguous

#else

#define ATTRS 

#endif

module data_types
   implicit none
   public

   type data_struct
      real, dimension(:, :), pointer ATTRS :: data => null()
      integer, dimension(2) :: dimensionSizes
      real :: defaultValue
      type (data_struct), pointer :: self_ptr => null()
   end type data_struct

   type (data_struct), pointer :: globalStruct

   contains

end module data_types

module setup_routines

   use data_types

   implicit none
   public

   contains

   subroutine allocate_data_type( in_data )

      type (data_struct), pointer :: in_data

      if ( associated(in_data) ) then
         return
      end if

      write(0, *) ' Allocating DDT'


      allocate(in_data)

      in_data % defaultValue = 0.0
      in_data % dimensionSizes(1) = DIM1SIZE
      in_data % dimensionSizes(2) = DIM2SIZE

      in_data % self_ptr => in_data

   end subroutine allocate_data_type

   subroutine allocate_sub_data( in_data )
      
      type (data_struct), pointer :: in_data

      if ( .not. associated(in_data) ) then
         return
      end if

      if ( associated(in_data % data) ) then
         return
      end if

      write(0, *) ' Allocating data pointer within DDT'
      allocate( in_data % data( in_data % dimensionSizes(2), in_data % dimensionSizes(1) ) )

   end subroutine allocate_sub_data

   subroutine deallocate_sub_data( in_data )

      type (data_struct), pointer :: in_data

      if ( .not. associated(in_data) ) then
         return
      end if

      if ( .not. associated(in_data % data) ) then
         return
      end if

      write(0, *) ' Deallocating data pointer within DDT'
      deallocate( in_data % data )

   end subroutine deallocate_sub_data

end module setup_routines

module test_routines

   use data_types

   implicit none
   public

   contains

   subroutine implicit_loop_test( in_data )

      type ( data_struct ), pointer :: in_data

      write(0, *) ' Performing implicit loop test'
      in_data % data(:, :) = in_data % defaultValue

   end subroutine implicit_loop_test

   subroutine explicit_loop_test( in_data )

      type ( data_struct ), pointer :: in_data

      integer :: iDim1, iDim2

      write(0, *) ' Performing explicit loop test with pointer loop bounds'
      do iDim1 = 1, in_data % dimensionSizes(1)
#ifdef _CONCUR
         do concurrent ( iDim2 = 1:in_data % dimensionSizes(2) )
            in_data % data( iDim2, iDim1 ) = in_data % defaultValue
         end do
#else
         do iDim2 = 1, in_data % dimensionSizes(2)
            in_data % data( iDim2, iDim1 ) = in_data % defaultValue
         end do
#endif
      end do

   end subroutine explicit_loop_test

   subroutine explicit_local_loop_test( in_data )

       type (data_struct), pointer :: in_data

       integer :: dim1Size, dim2Size
       integer :: iDim1, iDim2

       dim1Size = in_data % dimensionSizes(1)
       dim2Size = in_data % dimensionSizes(2)

      write(0, *) ' Performing explicit loop test with scalar loop bounds'
       do iDim1 = 1, dim1Size
#ifdef _CONCUR
          do concurrent ( iDim2 = 1:dim2Size )
             in_data % data(iDim2, iDim1) = in_data % defaultValue
          end do
#else
          do iDim2 = 1, dim2Size
             in_data % data(iDim2, iDim1) = in_data % defaultValue
          end do
#endif
       end do

   end subroutine explicit_local_loop_test

   subroutine deref_default_explicit_local_loop_test( in_data )

       type (data_struct), pointer :: in_data

       real :: tmpReal
       integer :: dim1Size, dim2Size
       integer :: iDim1, iDim2

       dim1Size = in_data % dimensionSizes(1)
       dim2Size = in_data % dimensionSizes(2)
       tmpReal = in_data % defaultValue

      write(0, *) ' Performing explicit loop test with scalar loop bounds and dereferenced default value'
       do iDim1 = 1, dim1Size
#ifdef _CONCUR
          do concurrent ( iDim2 = 1:dim2Size )
             in_data % data(iDim2, iDim1) = tmpReal
          end do
#else
          do iDim2 = 1, dim2Size
             in_data % data(iDim2, iDim1) = tmpReal
          end do
#endif
       end do

   end subroutine deref_default_explicit_local_loop_test

   subroutine full_deref_explicit_local_loop_test( in_data )

       type (data_struct), pointer :: in_data

       real, dimension(:, :), pointer ATTRS:: data_ptr
       real :: tmpReal
       integer :: dim1Size, dim2Size
       integer :: iDim1, iDim2

       dim1Size = in_data % dimensionSizes(1)
       dim2Size = in_data % dimensionSizes(2)
       tmpReal = in_data % defaultValue
       data_ptr => in_data % data

      write(0, *) ' Performing explicit loop test with scalar loop bounds and dereferenced default value, and references to data pointer instead of DDT'
       do iDim1 = 1, dim1Size
#ifdef _CONCUR
          do concurrent ( iDim2 = 1:dim2Size )
             data_ptr(iDim2, iDim1) = tmpReal
          end do
#else
          do iDim2 = 1, dim2Size
             data_ptr(iDim2, iDim1) = tmpReal
          end do
#endif
       end do

   end subroutine full_deref_explicit_local_loop_test

end module test_routines

program vector_test

   use data_types
   use setup_routines
   use test_routines

   implicit none

   type (data_struct), pointer :: data_ptr1

   integer :: testNum

   write(0, *) 'Starting test program'

   call allocate_data_type( globalStruct )

   data_ptr1 => globalStruct

   call allocate_sub_data( data_ptr1 )

   write(0, *) ''

   do testNum = 1, NUMTESTS

      write(0, *) 'Performing test # ', testNum

      call implicit_loop_test( data_ptr1 )
      call explicit_loop_test( data_ptr1 )
      call explicit_local_loop_test( data_ptr1 )
      call deref_default_explicit_local_loop_test( data_ptr1 )
      call full_deref_explicit_local_loop_test( data_ptr1 )

      write(0, *) 'End of test # ', testNum

   end do

   write(0, *) ''

   call deallocate_sub_data( data_ptr1 )

   nullify( data_ptr1 )
   deallocate( globalStruct )

   write(0, *) ' Finished test program'

   stop

end program vector_test
