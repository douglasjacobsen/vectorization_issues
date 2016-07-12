module data_types
   implicit none
   public

   type data_struct
      real, pointer, dimension(:, :) :: data => null()
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

      allocate(in_data)

      in_data % defaultValue = 0.0
      in_data % dimensionSizes(1) = 300
      in_data % dimensionSizes(2) = 100

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

      in_data % data(:, :) = in_data % defaultValue

   end subroutine implicit_loop_test

   subroutine explicit_loop_test( in_data )

      type ( data_struct ), pointer :: in_data

      integer :: iDim1, iDim2

      do iDim1 = 1, in_data % dimensionSizes(1)
         do iDim2 = 1, in_data % dimensionSizes(2)
            in_data % data( iDim2, iDim1 ) = in_data % defaultValue
         end do
      end do

   end subroutine explicit_loop_test

   subroutine explicit_local_loop_test( in_data )

       type (data_struct), pointer :: in_data

       integer :: dim1Size, dim2Size
       integer :: iDim1, iDim2

       dim1Size = in_data % dimensionSizes(1)
       dim2Size = in_data % dimensionSizes(2)

       do iDim1 = 1, dim1Size
          do iDim2 = 1, dim2Size
             in_data % data(iDim2, iDim1) = in_data % defaultValue
          end do
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

       do iDim1 = 1, dim1Size
          do iDim2 = 1, dim2Size
             in_data % data(iDim2, iDim1) = tmpReal
          end do
       end do

   end subroutine deref_default_explicit_local_loop_test

   subroutine full_deref_explicit_local_loop_test( in_data )

       type (data_struct), pointer :: in_data

       real, dimension(:, :), pointer :: data_ptr
       real :: tmpReal
       integer :: dim1Size, dim2Size
       integer :: iDim1, iDim2

       dim1Size = in_data % dimensionSizes(1)
       dim2Size = in_data % dimensionSizes(2)
       tmpReal = in_data % defaultValue
       data_ptr => in_data % data

       do iDim1 = 1, dim1Size
          do iDim2 = 1, dim2Size
             data_ptr(iDim2, iDim1) = tmpReal
          end do
       end do

   end subroutine full_deref_explicit_local_loop_test

end module test_routines

program vector_test

   use data_types
   use setup_routines
   use test_routines

   implicit none

   type (data_struct), pointer :: data_ptr1

   real :: tmpReal
   integer :: dimSize1, dimSize2
   integer :: iDim1, iDim2

   call allocate_data_type( globalStruct )

   data_ptr1 => globalStruct

   call allocate_sub_data( data_ptr1 )

   call implicit_loop_test( data_ptr1 )
   call explicit_loop_test( data_ptr1 )
   call explicit_local_loop_test( data_ptr1 )
   call deref_default_explicit_local_loop_test( data_ptr1 )
   call full_deref_explicit_local_loop_test( data_ptr1 )

   call deallocate_sub_data( data_ptr1 )

   nullify( data_ptr1 )
   deallocate( globalStruct )

   stop

end program vector_test
