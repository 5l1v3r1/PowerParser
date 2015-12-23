module assert_module

public assertTrue
public assertFalse
public assertEqual
public assertLessThanOrEqual

interface assertEqual
   module procedure assertEqualScalarInt
   module procedure assertEqualVectorInt
   module procedure assertEqualScalarReal
   module procedure assertEqualScalarRealTolerance
   module procedure assertEqualVectorReal
   module procedure assertEqualCharacter
end interface assertEqual

private

#ifndef INT4_KIND_DIGITS
#define INT4_KIND_DIGITS 6
#endif

#ifndef INT8_KIND_DIGITS
#define INT8_KIND_DIGITS 16
#endif

#ifndef REAL4_KIND_DIGITS
#define REAL4_KIND_DIGITS 6
#endif

#ifndef REAL8_KIND_DIGITS
#define REAL8_KIND_DIGITS 12
#endif

   integer, parameter :: INT4   = SELECTED_INT_KIND(INT4_KIND_DIGITS)
   integer, parameter :: INT32  = SELECTED_INT_KIND(INT4_KIND_DIGITS)

   integer, parameter :: INT8   = SELECTED_INT_KIND(INT8_KIND_DIGITS)
   integer, parameter :: INT64  = SELECTED_INT_KIND(INT8_KIND_DIGITS)

   integer, parameter :: REAL4  = SELECTED_REAL_KIND(REAL4_KIND_DIGITS)
   integer, parameter :: REAL32 = SELECTED_REAL_KIND(REAL4_KIND_DIGITS)

   integer, parameter :: REAL8  = SELECTED_REAL_KIND(REAL8_KIND_DIGITS)
   integer, parameter :: REAL64 = SELECTED_REAL_KIND(REAL8_KIND_DIGITS)


contains

subroutine assertTrue(l, string)
   logical, intent(in) :: l
   character(*), intent(in) :: string
   integer :: mype = 0
   integer :: iope = 0
   integer :: numpe = 1
   call parser_comm_info(mype, numpe, iope)

   if (l .eqv. .true.) then
      if (mype .eq. 0) write (*,'("PASSED: ",a)') trim(string)
   else
      if (mype .eq. 0) write (*,'("  FAILED: ",a)') trim(string)
   endif
end subroutine assertTrue

subroutine assertFalse(l, string)
   logical, intent(in) :: l
   character(*), intent(in) :: string
   integer :: mype = 0
   integer :: iope = 0
   integer :: numpe = 1
   call parser_comm_info(mype, numpe, iope)

   if (l .eqv. .false.) then
      if (mype .eq. 0) write (*,'("PASSED: ",a)') trim(string)
   else
      if (mype .eq. 0) write (*,'("  FAILED: ",a)') trim(string)
   endif
end subroutine assertFalse

subroutine assertEqualScalarInt(CorrectValue, TestValue, string)
   integer, intent(in) :: CorrectValue, TestValue
   character(*), intent(in) :: string
   integer :: mype = 0
   integer :: iope = 0
   integer :: numpe = 1
   call parser_comm_info(mype, numpe, iope)

   if (CorrectValue .eq. TestValue) then
      if (mype .eq. 0) write (*,'("PASSED: ",a)') trim(string)
   else
      if (mype .eq. 0) write (*,'("  FAILED: ",a, " CorrectValue ",a," TestValue ",a)') &
                              trim(string), CorrectValue, TestValue
   endif
end subroutine assertEqualScalarInt

subroutine assertEqualVectorInt(CorrectValue, TestValue, string)
   integer, intent(in) :: CorrectValue(:), TestValue(:)
   character(*), intent(in) :: string
   logical :: err = .false.
   integer :: mype = 0
   integer :: iope = 0
   integer :: numpe = 1
   call parser_comm_info(mype, numpe, iope)

   do i = 1,size(CorrectValue)
      if (CorrectValue(i) .ne. TestValue(i)) then
         err = .true.
      endif
   enddo

   if (.not. err) then
      if (mype .eq. 0) write (*,'("PASSED: ",a)') trim(string)
   else
      if (mype .eq. 0) write (*,'("  FAILED: ",a)') trim(string)
   endif
end subroutine assertEqualVectorInt

subroutine assertEqualScalarReal(CorrectValue, TestValue, string)
   real(REAL64), intent(in) :: CorrectValue, TestValue
   character(*), intent(in) :: string
   integer :: mype = 0
   integer :: iope = 0
   integer :: numpe = 1
   call parser_comm_info(mype, numpe, iope)

   if (CorrectValue .eq. TestValue) then
      if (mype .eq. 0) write (*,'("PASSED: ",a)') trim(string)
   else
      if (mype .eq. 0) write (*,'("  FAILED: ",a)') trim(string)
   endif
end subroutine assertEqualScalarReal

subroutine assertEqualScalarRealTolerance(CorrectValue, TestValue, Tolerance, string)
   real(REAL64), intent(in) :: CorrectValue, TestValue, Tolerance
   character(*), intent(in) :: string
   integer :: mype = 0
   integer :: iope = 0
   integer :: numpe = 1
   call parser_comm_info(mype, numpe, iope)

   if (abs(CorrectValue - TestValue) .le. Tolerance) then
      if (mype .eq. 0) write (*,'("PASSED: ",a)') trim(string)
   else
      if (mype .eq. 0) write (*,'("  FAILED: ",a)') trim(string)
   endif
end subroutine assertEqualScalarRealTolerance

subroutine assertEqualVectorReal(CorrectValue, TestValue, string)
   real(REAL64), intent(in) :: CorrectValue(:), TestValue(:)
   character(*), intent(in) :: string
   logical :: err = .false.
   integer :: mype = 0
   integer :: iope = 0
   integer :: numpe = 1
   call parser_comm_info(mype, numpe, iope)

   do i = 1,size(CorrectValue)
      if (CorrectValue(i) .ne. TestValue(i)) then
         err = .true.
      endif
   enddo

   if (.not. err) then
      if (mype .eq. 0) write (*,'("PASSED: ",a)') trim(string)
   else
      if (mype .eq. 0) write (*,'("  FAILED: ",a)') trim(string)
   endif
end subroutine assertEqualVectorReal

subroutine assertEqualCharacter(CorrectValue, TestValue, string)
   character(*), intent(in) :: CorrectValue, TestValue
   character(*), intent(in) :: string
   integer :: mype = 0
   integer :: iope = 0
   integer :: numpe = 1
   call parser_comm_info(mype, numpe, iope)

   if (CorrectValue .eq. TestValue) then
      if (mype .eq. 0) write (*,'("PASSED: ",a)') trim(string)
   else
      if (mype .eq. 0) write (*,'("  FAILED: ",a)') trim(string)
   endif
end subroutine assertEqualCharacter

subroutine assertLessThanOrEqual(CorrectValue, TestValue, string)
   real(REAL64), intent(in) :: CorrectValue, TestValue
   character(*), intent(in) :: string
   integer :: mype = 0
   integer :: iope = 0
   integer :: numpe = 1
   call parser_comm_info(mype, numpe, iope)

   if (CorrectValue .le. TestValue) then
      if (mype .eq. 0) write (*,'("PASSED: ",a)') trim(string)
   else
      if (mype .eq. 0) write (*,'("  FAILED: ",a)') trim(string)
   endif
end subroutine assertLessThanOrEqual

end module assert_module
