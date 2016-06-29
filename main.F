program vector_test

   implicit none

   type data_struct
      real, pointer, dimension(:, :) :: data
      integer, dimension(2) :: dimensionSizes
      real :: defaultValue
   end type data_struct

   type (data_struct), target :: initial_data
   type (data_struct), pointer :: data_ptr

   real :: tmpReal
   integer :: dimSize1, dimSize2
   integer :: iDim1, iDim2

   initial_data % dimensionSizes(1) = 300
   initial_data % dimensionSizes(2) = 100

   initial_data % defaultValue = 0.0

   data_ptr => initial_data

   allocate( data_ptr % data( data_ptr % dimensionSizes(2), data_ptr % dimensionSizes(1) ) )

   ! Implicit Loop
   data_ptr % data(:, :) = data_ptr % defaultValue

   dimSize1 = data_ptr % dimensionSizes(1)
   dimSize2 = data_ptr % dimensionSizes(2)

   ! Explicit Loop
   do iDim1 = 1, data_ptr % dimensionSizes(1)
      do iDim2 = 1, data_ptr % dimensionSizes(2)
         data_ptr % data(iDim2, iDim1) = data_ptr % defaultValue
      end do
   end do

   ! Explicit Loop with local for default value
   tmpReal = data_ptr % defaultValue
   do iDim1 = 1, data_ptr % dimensionSizes(1)
      do iDim2 = 1, data_ptr % dimensionSizes(2)
         data_ptr % data(iDim2, iDim1) = tmpReal
      end do
   end do

   deallocate( data_ptr % data )

   nullify(data_ptr)

   stop

end program vector_test
