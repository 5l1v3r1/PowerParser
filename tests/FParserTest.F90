program FParserTest

   use FParser_module
   use assert_module

   implicit none


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

   integer :: mype
   integer :: numpe
   integer :: iope
   integer :: maxpe

   real(REAL64), parameter :: PI        = 3.14159265358979323846_REAL64
   ! Speed of light in a vacuum (exact)
   real(REAL64), parameter :: SOL           = 2.997924580E+10_REAL64 ! [cm/s]
   real(REAL64), parameter :: ZERO = 0.0_REAL64

   character(24) :: Package = " Package:: Parser:"
   character(24) :: Module  = " Module:: QueryFParser:"
   character(120) :: msg

   character(20) :: deckname_arg = "fparsetest.in"
   character(20) :: oargs = " "
   integer :: check_input = 0

   ! local variables
   integer :: i, n, exearg_cmd01
   integer :: ierr

   logical :: some_logical_cmd
   real(REAL8) :: exp_val, denom, rdiff, volume_cmd

   logical :: sp_logical_array(10), mult_logical_array(6)
   integer :: math_result1, math_result3, math_result4, math_result5
   integer :: math_result5B
   integer :: math_result6, math_result7, math_result8, math_result9
   integer :: math_result10, upm01, upm02
   real(REAL8) :: math_result2, math_result11, math_result12
   integer :: int_array(4), pint_nested(10)
   real(REAL8) :: negnum, xcenter, cmdml(5), f01, f02
   real(REAL8) :: acmd, acmd2, acmd3(5), acmd4, acmd5(8)
   real(REAL8) :: depcmd01(5)
   real(REAL8), dimension(:), allocatable :: acontline
   integer, dimension(:), allocatable :: skip1d
   integer :: acontline_size, skip1d_size
   integer :: oddstrs_size
   logical :: good, idefvar_cmd01, idefvar_cmd02
   real(REAL8) :: idefvar_cmd03, idefvar_cmd04

   real(REAL8) :: skip_check_cmd(2)

   real(REAL8) :: var1d_res, var2d_res, vnc_cmd
   integer :: var8d_cmd, var8d_cmd2
   logical :: log1d_cmd
   character(24) :: vchar3d_cmd

   logical :: in_input, in_whenthen

   logical :: math_result13, math_result14, math_result15
   logical :: math_result16, math_result17, math_result18

   logical, dimension(:),       allocatable :: logical_array
   logical, dimension(:,:),     allocatable :: log2d
   logical, dimension(:,:,:),   allocatable :: log3d
   logical, dimension(:,:,:,:), allocatable :: log4d
   integer :: logical_array_size, log2d_size, log3d_size, log4d_size

   real(REAL8)                                  :: a2d(3,2)
   real(REAL8), dimension(:,:,:), allocatable   :: a3d
   real(REAL8), dimension(:,:,:,:), allocatable :: a4d
   integer :: a2d_size, a3d_size, a4d_size

   integer, dimension(:,:),     allocatable :: i2d
   integer, dimension(:,:,:),   allocatable :: i3d
   integer, dimension(:,:,:,:), allocatable :: i4d
   integer :: i2d_size, i3d_size, i4d_size

   character :: single_char, single_charq
   character(24) :: title, char1d(6), c1d_mult(5)
   character(24), dimension(:), allocatable :: oddstrs
   character(24), dimension(:,:), allocatable     :: char2d
   character(24), dimension(:,:,:), allocatable   :: char3d
   character(24), dimension(:,:,:,:), allocatable :: char4d
   integer :: char2d_size, char3d_size, char4d_size

   real(REAL8) :: delta_y_cmd01, delta_y_cmd02, delta_y_cmd03
   integer :: delta_y_cmd04
   real(REAL8) :: delta_y_cmd05, delta_y_cmd06, delta_y_cmd07
   real(REAL8) :: delta_x_cmd01, delta_x_cmd02, delta_x_cmd03

   integer :: do_sum_cmd01, do_sum_cmd02, do_sum_cmd03, do_sum_cmd04
   integer :: do_sum_cmd05, do_sum_cmd06, do_sum_cmd07

   integer :: sub_cmd01, sub_cmd02, sub_cmd03, sub_cmd04, sub_cmd05
   integer :: sub_cmd06(4), sub_cmd07, sub_cmd08, sub_cmd09
   integer :: sub_cmd10

   real(REAL8) :: inc_cmd01, inc_cmd02, inc_cmd03, inc_cmd04

   real(REAL8), dimension(:), allocatable     :: asm1d
   real(REAL8), dimension(:,:), allocatable   :: asm2d, mults
   integer :: asm1d_size, asm2d_size
   integer :: mults_size1, mults_size2

   integer :: iarith_cmd01, iarith_cmd02, iarith_cmd03

   integer :: strlen_cmd01, strlen_cmd02
   character(24) :: strcat_cmd01, strcat_cmd02, strerase_cmd01
   character(24) :: strerase_cmd02, strinsert_cmd01
   character(24) :: strsubstr_cmd01, strtrim_cmd01

   real(REAL8) :: matdef(200, 99)
   integer :: matreg(99), matreg2(99), nummat, numreg, numreg2

   integer :: ppmm_cmd01, ppmm_cmd02, ppmm_cmd03, ppmm_cmd04
   integer :: ppmm_cmd05, ppmm_cmd06, ppmm_cmd07, ppmm_cmd08
   integer :: ppmm_cmd09, ppmm_cmd10, ppmm_cmd11
   real(REAL8) :: ppmm_cmd12, ppmm_cmd13
   integer :: ppmm_cmd14
   real(REAL8) :: ppmm_cmd15, ppmm_cmd16

   logical :: rb_check
   integer :: rb_ntriggered, rb_num
   integer, dimension(:), allocatable :: rb_triggered_indices

   integer :: shortmodcyc, wtnum, modcyc, ncycle, wt_cmd04, wt_cmd05
   integer :: num_wt_cyc, wt_cmd06
   logical :: wt_check, wttf_c01, wt_cmd01, wt_cmd03
   real(REAL8) :: sim_time, wt_cmd02, sim_pressure(5)
   character(24) :: wttf_c02
   character(24), dimension(5) :: code_varnames, code_values
   integer, dimension(5) :: code_vv_active
   integer :: wt_casize = 0, max_casize
   character(1), dimension(:), allocatable :: wt_ca
   integer :: wt_satsize = 0, max_satsize
   integer, dimension(:), allocatable :: wt_sat

   integer :: quad_root1, quad_root2

   ! Passing C strings to Fortran90 is fraught with danger, so pass
   ! them in as character arrays and copy the contents into Fortran
   ! strings

   ! Initialize the QueryFParser user input system - for getting user input.
   !call QueryFParsera(fname, .false.)
   call FParser_initialize(.true.,deckname_arg,oargs)

   ! Add dictionary entries that are "pre-defined" -- third argument is
   ! true and called before the compile_buffer operations
   call FParser_dictionary_add("$sol",SOL,.true.,"Speed of light (cm/s)")
   ! PI is defined in define_kind
   call FParser_dictionary_add("$pi", PI, .true.,"Circle circumference/diameter")

   call FParser_compile_buffer()

   ! Get processor info.
   !     mype  = id for this processor (0 to numpe-1)
   !     iope  = id for the i/o processor (0 to numpe-1 -- normally 0)
   !     numpe = number of processors
   mype = 0
   iope = 0
   numpe = 1
   call parser_comm_info(mype, numpe, iope)
   maxpe = numpe-1

   ! Open the output file.
   open(12, FILE="test_output", STATUS='REPLACE')

   ! Check for duplicate scalar values.
   ! Echo the input file, vars, functions, final buffer.
   call parser_chk_scalar_dup
   call FParser_echo_user_input(12)
   !call FParser_echo_fvf(12)

   ! Initialize the unit test package.

   ! ---------------------------------------------------------------------------
   ! ---------------------------------------------------------------------------
   ! Logical data.

   ! Test simple logical command.
   some_logical_cmd = .false.
   call QueryFParser('some_logical_cmd', some_logical_cmd, .true.)
   call assertTrue(some_logical_cmd, trim(Package)//trim(Module)//" simple logical command")

    ! Test one dimension logical command array.
    msg = trim(Package)//trim(Module)//" logical single dimension array"
    call FParser_size('logical_array', logical_array_size)
    call assertEqual(10, logical_array_size, msg)
    allocate(logical_array(logical_array_size))
    logical_array = .false.
    call QueryFParser('logical_array', logical_array, .true., logical_array_size)
    call assertFalse(logical_array(1), msg)
    call assertTrue(logical_array(2), msg)
    call assertTrue(logical_array(3), msg)
    call assertFalse(logical_array(4), msg)
    call assertTrue(logical_array(5), msg)
    call assertTrue(logical_array(6), msg)
    call assertFalse(logical_array(7), msg)
    call assertTrue(logical_array(8), msg)
    call assertTrue(logical_array(9), msg)
    call assertTrue(logical_array(10), msg)
    deallocate(logical_array)

    ! Test one dimension logical command array, space delimited with a
    ! comment line between the array values.
    sp_logical_array = .false.
    msg = trim(Package)//trim(Module)//" logical array, space delimited"
    call QueryFParser('sp_logical_array', sp_logical_array, .true., 10)
    call assertFalse(sp_logical_array(1), msg)
    call assertTrue(sp_logical_array(2), msg)
    call assertTrue(sp_logical_array(3), msg)
    call assertFalse(sp_logical_array(4), msg)
    call assertTrue(sp_logical_array(5), msg)
    call assertTrue(sp_logical_array(6), msg)
    call assertFalse(sp_logical_array(7), msg)
    call assertTrue(sp_logical_array(8), msg)
    call assertTrue(sp_logical_array(9), msg)
    call assertTrue(sp_logical_array(10), msg)

    ! Test 1D logical array with multiplicity.
    mult_logical_array = .false.
    msg = trim(Package)//trim(Module)//" logical array, multiplicity"
    call QueryFParser('mult_logical_array', mult_logical_array, .true., 6)
    call assertFalse(mult_logical_array(1), msg)
    call assertFalse(mult_logical_array(2), msg)
    call assertFalse(mult_logical_array(3), msg)
    call assertTrue(mult_logical_array(4), msg)
    call assertTrue(mult_logical_array(5), msg)
    call assertTrue(mult_logical_array(6), msg)

    ! A two dimensional logical array. We test FParser_size and allocate memory.
    msg = trim(Package)//trim(Module)//" log2d, 2d logical array"
    call FParser_size('log2d', 3, log2d_size)
    call assertEqual(2, log2d_size, msg)
    allocate(log2d(3,log2d_size))
    log2d = .false.
    call QueryFParser('log2d', log2d, .true., 3, log2d_size)
    call assertTrue(log2d(1,1), msg)
    call assertTrue(log2d(2,1), msg)
    call assertFalse(log2d(3,1), msg)
    call assertTrue(log2d(1,2), msg)
    call assertFalse(log2d(2,2), msg)
    call assertFalse(log2d(3,2), msg)

    ! A three dimensional logical array. We test FParser_size and allocate memory.
    msg = trim(Package)//trim(Module)//" log3d, 3d logical array"
    call FParser_size('log3d', 3, 2, log3d_size)
    call assertEqual(3, log3d_size, msg)
    allocate(log3d(3,2,log3d_size))
    log3d = .false.
    call QueryFParser('log3d', log3d, .true., 3, 2, log3d_size)
    call assertTrue(log3d(1,1,1), msg)
    call assertFalse(log3d(2,1,1), msg)
    call assertFalse(log3d(3,1,1), msg)
    call assertFalse(log3d(1,2,1), msg)
    call assertTrue(log3d(2,2,1), msg)
    call assertFalse(log3d(3,2,1), msg)

    call assertTrue(log3d(1,1,2), msg)
    call assertFalse(log3d(2,1,2), msg)
    call assertFalse(log3d(3,1,2), msg)
    call assertFalse(log3d(1,2,2), msg)
    call assertTrue(log3d(2,2,2), msg)
    call assertFalse(log3d(3,2,2), msg)

    call assertTrue(log3d(1,1,3), msg)
    call assertTrue(log3d(2,1,3), msg)
    call assertTrue(log3d(3,1,3), msg)
    call assertFalse(log3d(1,2,3), msg)
    call assertFalse(log3d(2,2,3), msg)
    call assertTrue(log3d(3,2,3), msg)

    ! A four dimensional logical array. We test FParser_size and allocate memory.
    msg = trim(Package)//trim(Module)//" log4d, 4d logical array"
    call FParser_size('log4d', 2, 1, 3, log4d_size)
    call assertEqual(2,log4d_size,msg)
    allocate(log4d(2,1,3,log4d_size))
    log4d = .false.
    call QueryFParser('log4d', log4d, .true., 2, 1, 3, log4d_size)
    call assertTrue(log4d(1,1,1,1), msg)
    call assertTrue(log4d(2,1,1,1), msg)
    call assertFalse(log4d(1,1,2,1), msg)
    call assertFalse(log4d(2,1,2,1), msg)
    call assertTrue(log4d(1,1,3,1), msg)
    call assertTrue(log4d(2,1,3,1), msg)
    call assertTrue(log4d(1,1,1,2), msg)
    call assertFalse(log4d(2,1,1,2), msg)
    call assertFalse(log4d(1,1,2,2), msg)
    call assertFalse(log4d(2,1,2,2), msg)
    call assertFalse(log4d(1,1,3,2), msg)
    call assertFalse(log4d(2,1,3,2), msg)

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Math checks

    ! Check math_result1 as an integer.
    math_result1 = 0
    call QueryFParser('math_result1', math_result1, .true.)
    call assertEqual(7, math_result1, trim(Package)//trim(Module)//" math_result1, 1+2*3")

    ! Check math_result2 as a REAL8
    math_result2 = 0._REAL8
    call QueryFParser('math_result2', math_result2, .true.)
    call assertEqual(3.8_REAL8,  math_result2, trim(Package)//trim(Module)//" math_result2, 1.1+2.7*2.0/2")

    ! Check math_result3 as an integer.
    math_result3 = 0
    call QueryFParser('math_result3', math_result3, .true.)
    call assertEqual(6,  math_result3, trim(Package)//trim(Module)//" math_result3, fmod(15,4)+3")

    ! Check math_result8 as an integer.
    math_result8 = 0
    call QueryFParser('math_result8', math_result8, .true.)
    call assertEqual(49,  math_result8, trim(Package)//trim(Module)//" math_result8, 13+(4*(15/3+4))")

    ! Check math_result9 as an integer.
    math_result9 = 0
    call QueryFParser('math_result9', math_result9, .true.)
    call assertEqual(5,  math_result9, trim(Package)//trim(Module)//" math_result9, sin+cos")

    ! Check math_result10 as an integer.
    math_result10 = 0
    call QueryFParser('math_result10', math_result10, .true.)
    call assertEqual(1,  math_result10, trim(Package)//trim(Module)//" math_result10, sin**2+cos**2")

    ! Exponentiation with **
    math_result11 = ZERO
    call QueryFParser('math_result11', math_result11, .true.)
    call assertEqual(64.0_REAL8,  math_result11, trim(Package)//trim(Module)//" math_result11, 4**3")

    ! sin**2 + cos**2 using the ** operator
    math_result12 = ZERO
    call QueryFParser('math_result12', math_result12, .true.)
    call assertEqual(1.0_REAL8,  math_result12, trim(Package)//trim(Module)//" math_result11, sin**2+cos**2, use ** op")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Unary plus and minus
    ! Check math_result4 as an integer.
    math_result4 = 0
    call QueryFParser('math_result4', math_result4, .true.)
    call assertEqual(-5,  math_result4, trim(Package)//trim(Module)//" math_result4, -5")

    ! Check math_result5 as an integer.
    math_result5 = 0
    call QueryFParser('math_result5', math_result5, .true.)
    call assertEqual(9,  math_result5, trim(Package)//trim(Module)//" math_result5, 4--5")

    ! Unary +
    math_result5B = 0
    call QueryFParser('math_result5B', math_result5B, .true.)
    call assertEqual(9,  math_result5B, trim(Package)//trim(Module)//" math_result5B, 4++5")

    ! Check math_result6 as an integer.
    math_result6 = 0
    call QueryFParser('math_result6', math_result6, .true.)
    call assertEqual(-20,  math_result6, trim(Package)//trim(Module)//" math_result6, 4*-5")

    ! Check math_result7 as an integer.
    math_result7 = 0
    call QueryFParser('math_result7', math_result7, .true.)
    call assertEqual(-17,  math_result7, trim(Package)//trim(Module)//" math_result7, 3+(4*-5)")

    ! Unary plus and minus with variables
    upm01 = 0
    call QueryFParser('upm01', upm01, .true.)
    call assertEqual(8,  upm01, trim(Package)//trim(Module)//" upm01, unary - with vars")

    upm02 = 0
    call QueryFParser('upm02', upm02, .true.)
    call assertEqual(8,  upm02, trim(Package)//trim(Module)//" upm02, unary + with vars")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Some Miscellaneous Checks

    ! Check unary minus in the command line.
    exearg_cmd01 = 0
    call QueryFParser('exearg_cmd01', exearg_cmd01, .true.)
    call assertEqual(3,  exearg_cmd01, trim(Package)//trim(Module)//" exe args, exearg_cmd01")

    ! Check unary minus in the command line.
    volume_cmd = 0._REAL8
    call QueryFParser('volume_cmd', volume_cmd, .true.)
    exp_val = 7.2382294738709_REAL8
    denom = 0.5_REAL8 * (volume_cmd + exp_val)
    if (denom .eq. 0._REAL8) rdiff = 1.e30_REAL8
    if (denom .ne. 0._REAL8) rdiff = abs(volume_cmd - 7.2382294738709_REAL8) / denom
    call assertLessThanOrEqual(rdiff, 1.0e-10_REAL8, trim(Package)//trim(Module)//" $volume = 4/3 pi r**3")

    ! Check unary minus in the command line.
    negnum = 0._REAL8
    call QueryFParser('negnum', negnum, .true.)
    call assertEqual(-5.2_REAL8, negnum, trim(Package)//trim(Module)//" negnum, unary minus in cmd line")

    ! acmd real test
    acmd = 0._REAL8
    call QueryFParser('acmd', acmd, .true.)
    call assertEqual(1.34_REAL8, acmd, trim(Package)//trim(Module)//" acmd, acmd=$rho1=1.34")

    ! acmd2 start with .
    acmd2 = 0._REAL8
    call QueryFParser('acmd2', acmd2, .true.)
    call assertEqual(0.1_REAL8, acmd2, trim(Package)//trim(Module)//" acmd2, test start with .")

    ! Unary plus
    acmd3 = 0._REAL8
    call QueryFParser('acmd3', acmd3, .true., 5)
    call assertEqual((/0.1_REAL8, 3.0_REAL8, 4.0_REAL8, 4.0_REAL8, 4.0_REAL8/),acmd3, &
       trim(Package)//trim(Module)//" acmd3 real array, unary plus")

    ! E instead of e
    acmd4 = 0._REAL8
    call QueryFParser('acmd4', acmd4, .true.)
    call assertEqual(1.3e4_REAL8, acmd4, trim(Package)//trim(Module)//" acmd4, E instead of e")

    ! Test numbers starting with e or E
    acmd5 = 0._REAL8
    call QueryFParser('acmd5', acmd5, .true., 8)

    call assertEqual((/0.01_REAL8, 10.0_REAL8, -0.001_REAL8, 1000._REAL8, 1000._REAL8, 1000._REAL8, 10._REAL8, 100._REAL8/), &
        acmd5, trim(Package)//trim(Module)//" acmd5 real array, start with e or E")

    ! Test space between digits and e
    depcmd01 = 0._REAL8
    call QueryFParser('depcmd01', depcmd01, .true., 5)
    call assertEqual((/1.0_REAL8, 3.0e14_REAL8, -5.0_REAL8, 2.0_REAL8, -5.e15_REAL8/), depcmd01, &
       trim(Package)//trim(Module)//" depcmd01 space between digits and e")

    ! xcenter real test
    xcenter = 0._REAL8
    call QueryFParser('xcenter', xcenter, .false.)
    call assertEqual(0.0_REAL8, xcenter, trim(Package)//trim(Module)//" xcenter=1.0, noskip=.false.")

    ! Note that skip_check_cmd is only allocated for 2 values but 5 values are
    ! being requested in the QueryFParser call. This is ok (and is part of this check)
    ! because noskip is set to .false., thus the skip_check_cmd array is not
    ! filled, internal memory is not allocated, etc.
    skip_check_cmd = 0._REAL8
    call QueryFParser('skip_check_cmd', skip_check_cmd, .false., 5)
    call assertEqual(0.0_REAL8, skip_check_cmd(1), trim(Package)//trim(Module)//" skip_check_cmd, noskip=.false.")
    call assertEqual(0.0_REAL8, skip_check_cmd(2), trim(Package)//trim(Module)//" skip_check_cmd, noskip=.false.")

    ! Check for commands in the user input
    call FParser_cmd_in_input('var8d_cmd', in_input, in_whenthen)
    call assertTrue(in_input, trim(Package)//trim(Module)//" cmd in input, var8d_cmd")
    call assertFalse(in_whenthen, trim(Package)//trim(Module)//" cmd in input, var8d_cmd")

    call FParser_cmd_in_input('var800d_cmd', in_input, in_whenthen)
    call assertFalse(in_input, trim(Package)//trim(Module)//" cmd in input, var800d_cmd")
    call assertFalse(in_whenthen, trim(Package)//trim(Module)//" cmd in input, var800d_cmd")

    call FParser_cmd_in_input('wt_cmd01', in_input, in_whenthen)
    call assertFalse(in_input, trim(Package)//trim(Module)//" cmd in input, wt_cmd01")
    call assertTrue(in_whenthen, trim(Package)//trim(Module)//" cmd in input, wt_cmd01")

    call FParser_cmd_in_input('wt_cmd05', in_input, in_whenthen)
    call assertFalse(in_input, trim(Package)//trim(Module)//" cmd in input, wt_cmd05")
    call assertTrue(in_whenthen, trim(Package)//trim(Module)//" cmd in input, wt_cmd05")

    ! Mark commands as being processed.
    call FParser_cmd_set_processed('unused_cmd', .true.)
    call assertTrue(.true., trim(Package)//trim(Module)//" cmd set processed, unused_cmd")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Integer arithmetic
    iarith_cmd01 = 0
    call QueryFParser('iarith_cmd01', iarith_cmd01, .true.)
    call assertEqual(3, iarith_cmd01, trim(Package)//trim(Module)//" iarith_cmd01, fmod(15,4)")

    iarith_cmd02 = 0
    call QueryFParser('iarith_cmd02', iarith_cmd02, .true.)
    call assertEqual(3, iarith_cmd02, trim(Package)//trim(Module)//" iarith_cmd02, floor(15/4)")

    iarith_cmd03 = 0
    call QueryFParser('iarith_cmd03', iarith_cmd03, .true.)
    call assertEqual(4, iarith_cmd03, trim(Package)//trim(Module)//" iarith_cmd03, ceil(15/4)")


    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! ++ and -- operator checks
    ppmm_cmd01 = 0
    call QueryFParser('ppmm_cmd01', ppmm_cmd01, .true.)
    call assertEqual(5, ppmm_cmd01, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd01")

    ppmm_cmd02 = 0
    call QueryFParser('ppmm_cmd02', ppmm_cmd02, .true.)
    call assertEqual(6, ppmm_cmd02, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd02")

    ppmm_cmd03 = 0
    call QueryFParser('ppmm_cmd03', ppmm_cmd03, .true.)
    call assertEqual(6, ppmm_cmd03, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd03")

    ppmm_cmd04 = 0
    call QueryFParser('ppmm_cmd04', ppmm_cmd04, .true.)
    call assertEqual(5, ppmm_cmd04, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd04")

    ppmm_cmd05 = 0
    call QueryFParser('ppmm_cmd05', ppmm_cmd05, .true.)
    call assertEqual(11, ppmm_cmd05, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd05")

    ppmm_cmd06 = 0
    call QueryFParser('ppmm_cmd06', ppmm_cmd06, .true.)
    call assertEqual(19, ppmm_cmd06, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd06")

    ppmm_cmd07 = 0
    call QueryFParser('ppmm_cmd07', ppmm_cmd07, .true.)
    call assertEqual(20, ppmm_cmd07, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd07")

    ppmm_cmd08 = 0
    call QueryFParser('ppmm_cmd08', ppmm_cmd08, .true.)
    call assertEqual(6, ppmm_cmd08, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd08")

    ppmm_cmd09 = 0
    call QueryFParser('ppmm_cmd09', ppmm_cmd09, .true.)
    call assertEqual(7, ppmm_cmd09, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd09")

    ppmm_cmd10 = 0
    call QueryFParser('ppmm_cmd10', ppmm_cmd10, .true.)
    call assertEqual(20, ppmm_cmd10, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd10")

    ppmm_cmd11 = 0
    call QueryFParser('ppmm_cmd11', ppmm_cmd11, .true.)
    call assertEqual(19, ppmm_cmd11, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd11")

    ppmm_cmd12 = 0._REAL8
    call QueryFParser('ppmm_cmd12', ppmm_cmd12, .true.)
    call assertEqual(17.356_REAL8, ppmm_cmd12, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd12")

    ppmm_cmd13 = 0._REAL8
    call QueryFParser('ppmm_cmd13', ppmm_cmd13, .true.)
    call assertEqual(18.356_REAL8, ppmm_cmd13, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd13")

    ppmm_cmd14 = 0
    call QueryFParser('ppmm_cmd14', ppmm_cmd14, .true.)
    call assertEqual(5, ppmm_cmd14, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd14")

    ppmm_cmd15 = 0._REAL8
    call QueryFParser('ppmm_cmd15', ppmm_cmd15, .true.)
    call assertEqual(-320._REAL8, ppmm_cmd15, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd15")

    ppmm_cmd16 = 0._REAL8
    call QueryFParser('ppmm_cmd16', ppmm_cmd16, .true.)
    call assertEqual(33._REAL8, ppmm_cmd16, trim(Package)//trim(Module)//" ++ -- check, ppmm_cmd16")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! String operations.
    strlen_cmd01 = 0
    call QueryFParser('strlen_cmd01', strlen_cmd01, .true.)
    call assertEqual(39, strlen_cmd01, trim(Package)//trim(Module)//" strlen_cmd01, strlen(...)")

    strlen_cmd02 = 0
    call QueryFParser('strlen_cmd02', strlen_cmd02, .true.)
    call assertEqual(6, strlen_cmd02, trim(Package)//trim(Module)//" strlen_cmd02, strlen(1.0e14)")

    strcat_cmd01 = " ";
    call QueryFParser('strcat_cmd01', strcat_cmd01, .true., 24)
    call assertEqual("Obi-Wan Kenobi", strcat_cmd01, trim(Package)//trim(Module)//" strcat_cmd01, Obi-Wan Kenobi")

    strcat_cmd02 = " ";
    call QueryFParser('strcat_cmd02', strcat_cmd02, .true., 24)
    call assertEqual("Obi123", strcat_cmd02, trim(Package)//trim(Module)//" strcat_cmd02, Obi123")

    strerase_cmd01 = " ";
    call QueryFParser('strerase_cmd01', strerase_cmd01, .true., 24)
    call assertEqual("Force", strerase_cmd01, trim(Package)//trim(Module)//" strerase_cmd01, Force")

    strerase_cmd02 = " ";
    call QueryFParser('strerase_cmd02', strerase_cmd02, .true., 24)
    call assertEqual("1.e14", strerase_cmd02, trim(Package)//trim(Module)//" strerase_cmd02, 1.0e14")

    strinsert_cmd01 = " ";
    call QueryFParser('strinsert_cmd01', strinsert_cmd01, .true., 24)
    call assertEqual("Use The Force", strinsert_cmd01, trim(Package)//trim(Module)//" strinsert_cmd01, Use The Force")

    strsubstr_cmd01 = " ";
    call QueryFParser('strsubstr_cmd01', strsubstr_cmd01, .true., 24)
    call assertEqual("The", strsubstr_cmd01, trim(Package)//trim(Module)//" strsubstr_cmd01, Use The Force")

    strtrim_cmd01 = " ";
    call QueryFParser('strtrim_cmd01', strtrim_cmd01, .true., 24)
    call assertEqual("Use The Force, Scott", strtrim_cmd01, trim(Package)//trim(Module)//" strtrim_cmd01, trim a string")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Intrinsic functions
    f01 = 0._REAL8
    call QueryFParser('f01', f01, .true.)
    call assertEqual(4.58_REAL8, f01, 1.e-8_REAL8, trim(Package)//trim(Module)//" Function, f01")

    f02 = 0._REAL8
    call QueryFParser('f02', f02, .true.)
    call assertEqual(60.0_REAL8, f02, 1.e-8_REAL8, trim(Package)//trim(Module)//" Function, f02")

    ! defined function.
    idefvar_cmd01 = .false.
    call QueryFParser('idefvar_cmd01', idefvar_cmd01, .true.)
    call assertTrue(idefvar_cmd01, trim(Package)//trim(Module)//" variable defined or not, idefvar_cmd01")

    idefvar_cmd02 = .false.
    call QueryFParser('idefvar_cmd02', idefvar_cmd02, .true.)
    call assertTrue(idefvar_cmd02, trim(Package)//trim(Module)//" variable defined or not, idefvar_cmd02")

    idefvar_cmd03 = 0._REAL8
    call QueryFParser('idefvar_cmd03', idefvar_cmd03, .true.)
    call assertEqual(2.35_REAL8, idefvar_cmd03, trim(Package)//trim(Module)//" variable defined or not, idefvar_cmd03")

    idefvar_cmd04 = 0._REAL8
    call QueryFParser('idefvar_cmd04', idefvar_cmd04, .true.)
    call assertEqual(19.2_REAL8, idefvar_cmd04, trim(Package)//trim(Module)//" variable defined or not, idefvar_cmd04")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Relational and logical operators
    math_result13 = .true.
    call QueryFParser('math_result13', math_result13, .true.)
    call assertFalse(math_result13, trim(Package)//trim(Module)//" math_result13, 5.eq.2")

    math_result14 = .false.
    call QueryFParser('math_result14', math_result14, .true.)
    call assertTrue(math_result14, trim(Package)//trim(Module)//" math_result14, .not.false")

    math_result15 = .true.
    call QueryFParser('math_result15', math_result15, .true.)
    call assertFalse(math_result15, trim(Package)//trim(Module)//" math_result15, .not.true")

    math_result16 = .true.
    call QueryFParser('math_result16', math_result16, .true.)
    call assertFalse(math_result16, trim(Package)//trim(Module)//" math_result16, false.and.true")

    math_result17 = .false.
    call QueryFParser('math_result17', math_result17, .true.)
    call assertTrue(math_result17, trim(Package)//trim(Module)//" math_result17, 5.gt.4.or.10.gt.20.and.false")

    math_result18 = .true.
    call QueryFParser('math_result18', math_result18, .true.)
    call assertFalse(math_result18, trim(Package)//trim(Module)//" math_result18, (5.gt.4.or.10.gt.20.)and.false")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Multi-dimensional variables.
    var1d_res = 0._REAL8
    call QueryFParser('var1d_res', var1d_res, .true.)
    call assertEqual(30.0_REAL8, var1d_res, trim(Package)//trim(Module)//" var1d_res, 1d variable")

    var2d_res = 0._REAL8
    call QueryFParser('var2d_res', var2d_res, .true.)
    call assertEqual(320.0_REAL8, var2d_res, trim(Package)//trim(Module)//" var2d_res, 2d variable")

    var8d_cmd = 0
    call QueryFParser('var8d_cmd', var8d_cmd, .true.)
    call assertEqual(32121221, var8d_cmd, trim(Package)//trim(Module)//" var8d_cmd, 8d variable")

    var8d_cmd2 = 0
    call QueryFParser('var8d_cmd2', var8d_cmd2, .true.)
    call assertEqual(32121321, var8d_cmd2, trim(Package)//trim(Module)//" var8d_cmd2, use var not in parens")

    log1d_cmd = .false.
    call QueryFParser('log1d_cmd', log1d_cmd, .true.)
    call assertTrue(log1d_cmd, trim(Package)//trim(Module)//" log1d_cmd, logical variable 1d array")

    vchar3d_cmd = "whatever"
    call QueryFParser('vchar3d_cmd', vchar3d_cmd, .true., 24)
    call assertEqual("use", vchar3d_cmd, trim(Package)//trim(Module)//" vchar3d_cmd, char variable 3d array")

    vnc_cmd = 0._REAL8
    call QueryFParser('vnc_cmd', vnc_cmd, .true.)
    call assertEqual(2.0e19_REAL8, vnc_cmd, trim(Package)//trim(Module)//" vnc_cmd, variable 1.e19")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Multi-dimensional integer arrays.

    ! Test one dimension integer array.
    int_array = 0
    call QueryFParser('array', int_array, .true., 4)
    call assertEqual((/45,2,33,15/), int_array, trim(Package)//trim(Module)//" integer single dimension array")

    ! Another one dimension integer array. This one has nested
    ! comments in it. It also starts from array position 5.
    pint_nested = 0
    call QueryFParser('p', pint_nested, .true., 10)
    call assertEqual((/0,0,0,0,2,4,9,10,14,90/), pint_nested, trim(Package)//trim(Module)//" p int array, nested comment, start 5")

    ! A two dimensional integer array. We test FParser_size and allocate memory.
    msg = trim(Package)//trim(Module)//" i2d, 2d integer array"
    call FParser_size('i2d', 3, i2d_size)
    call assertEqual(2, i2d_size, msg)
    allocate(i2d(3,i2d_size))
    i2d = 1
    call QueryFParser('i2d', i2d, .true., 3, i2d_size)
    call assertEqual( 3, i2d(1,1), msg)
    call assertEqual( 4, i2d(2,1), msg)
    call assertEqual(-3, i2d(3,1), msg)
    call assertEqual( 5, i2d(1,2), msg)
    call assertEqual(-7, i2d(2,2), msg)
    call assertEqual(-7, i2d(3,2), msg)

    ! A three dimensional integer array. We also test FParser_size
    msg = trim(Package)//trim(Module)//" i3d, 3d integer array, allocate mem"
    call FParser_size('i3d', 3, 2, i3d_size)
    call assertEqual(3, i3d_size, msg)
    allocate(i3d(3,2,i3d_size))
    i3d = 1
    call QueryFParser('i3d', i3d, .true., 3, 2, i3d_size)
    call assertEqual(1        , i3d(1,1,1), msg)
    call assertEqual(1        , i3d(2,1,1), msg)
    call assertEqual(1        , i3d(3,1,1), msg)
    call assertEqual(1        , i3d(1,2,1), msg)
    call assertEqual(1        , i3d(2,2,1), msg)
    call assertEqual(1        , i3d(3,2,1), msg)

    call assertEqual(3        , i3d(1,1,2), msg)
    call assertEqual(4        , i3d(2,1,2), msg)
    call assertEqual(-4       , i3d(3,1,2), msg)
    call assertEqual(5        , i3d(1,2,2), msg)
    call assertEqual(-2       , i3d(2,2,2), msg)
    call assertEqual(-4       , i3d(3,2,2), msg)

    call assertEqual(3        , i3d(1,1,3), msg)
    call assertEqual(7        , i3d(2,1,3), msg)
    call assertEqual(-543     , i3d(3,1,3), msg)
    call assertEqual(62145679 , i3d(1,2,3), msg)
    call assertEqual(-19284213, i3d(2,2,3), msg)
    call assertEqual(2        , i3d(3,2,3), msg)

    ! A four dimensional integer array. We also test FParser_size
    msg = trim(Package)//trim(Module)//" i4d, 4d integer array, allocate mem"
    call FParser_size('i4d', 2, 1, 3, i4d_size)
    call assertEqual(2, i4d_size, msg)
    allocate(i4d(2,1,3,i4d_size))
    i4d = 1
    call QueryFParser('i4d', i4d, .true., 2, 1, 3, i4d_size)
    call assertEqual(3   , i4d(1,1,1,1), msg)
    call assertEqual(4   , i4d(2,1,1,1), msg)
    call assertEqual(-14 , i4d(1,1,2,1), msg)
    call assertEqual(49  , i4d(2,1,2,1), msg)
    call assertEqual(19  , i4d(1,1,3,1), msg)
    call assertEqual(42  , i4d(2,1,3,1), msg)

    call assertEqual(-3  , i4d(1,1,1,2), msg)
    call assertEqual(542 , i4d(2,1,1,2), msg)
    call assertEqual(-165, i4d(1,1,2,2), msg)
    call assertEqual(555 , i4d(2,1,2,2), msg)
    call assertEqual(199 , i4d(1,1,3,2), msg)
    call assertEqual(942 , i4d(2,1,3,2), msg)

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Multi-dimensional real arrays.

    ! A one dimension real array.
    cmdml = 1._REAL8
    msg = trim(Package)//trim(Module)//" cmdml real array, after mline comment"
    call QueryFParser('cmdml', cmdml, .true., 5)
    call assertEqual(1.0_REAL8  , cmdml(1), msg)
    call assertEqual(14.6_REAL8 , cmdml(2), msg)
    call assertEqual(17.8_REAL8 , cmdml(3), msg)
    call assertEqual(10.9_REAL8 , cmdml(4), msg)
    call assertEqual(1.e19_REAL8, cmdml(5), msg)

    ! Another 1 dimension real array. This tests the ability to use FParser_size
    ! to get the number of values first.
    msg = trim(Package)//trim(Module)//" acontline real array, use FParser_size"
    call FParser_size('acontline', acontline_size)
    call assertEqual(4, acontline_size, msg)
    allocate(acontline(acontline_size))
    acontline = 1.3_REAL8
    call QueryFParser('acontline', acontline, .true., acontline_size)
    call assertEqual(1.0_REAL8  , acontline(1), msg)
    call assertEqual(45.0_REAL8 , acontline(2), msg)
    call assertEqual(-15.0_REAL8, acontline(3), msg)
    call assertEqual(92.0_REAL8 , acontline(4), msg)
    deallocate(acontline)

    ! A two dimensional real array. We test FParser_size but do not allocate
    ! memory.
    msg = trim(Package)//trim(Module)//" a2d, 2d real array"
    call FParser_size('a2d', 3, a2d_size)
    call assertEqual(2, a2d_size, msg)
    a2d = 1._REAL8
    call QueryFParser('a2d', a2d, .true., 3, 2)
    call assertEqual( 3.0_REAL8    , a2d(1,1), msg)
    call assertEqual( 4.5_REAL8    , a2d(2,1), msg)
    call assertEqual(-3.0_REAL8    , a2d(3,1), msg)
    call assertEqual( 4.0e19_REAL8 , a2d(1,2), msg)
    call assertEqual(-3.0e-23_REAL8, a2d(2,2), msg)
    call assertEqual( 1.0_REAL8    , a2d(3,2), msg)

    ! A three dimensional real array. We also test FParser_size
    msg = trim(Package)//trim(Module)//" a3d, 3d real array, allocate mem"
    call FParser_size('a3d', 3, 2, a3d_size)
    call assertEqual(3, a3d_size, msg)
    allocate(a3d(3,2,a3d_size))
    a3d = 1._REAL8
    call QueryFParser('a3d', a3d, .true., 3, 2, a3d_size)
    call assertEqual(3.0_REAL8     , a3d(1,1,1), msg)
    call assertEqual(4.5_REAL8     , a3d(2,1,1), msg)
    call assertEqual(-3.0_REAL8    , a3d(3,1,1), msg)
    call assertEqual(4.0e19_REAL8  , a3d(1,2,1), msg)
    call assertEqual(-3.0e-23_REAL8, a3d(2,2,1), msg)
    call assertEqual(1.0_REAL8     , a3d(3,2,1), msg)
    call assertEqual(3.1_REAL8     , a3d(1,1,2), msg)
    call assertEqual(4.6_REAL8     , a3d(2,1,2), msg)
    call assertEqual(-4.0_REAL8    , a3d(3,1,2), msg)
    call assertEqual(5.0e19_REAL8  , a3d(1,2,2), msg)
    call assertEqual(-2.0e-23_REAL8, a3d(2,2,2), msg)
    call assertEqual(1.4_REAL8     , a3d(3,2,2), msg)
    call assertEqual(3.2_REAL8     , a3d(1,1,3), msg)
    call assertEqual(4.7_REAL8     , a3d(2,1,3), msg)
    call assertEqual(-5.0_REAL8    , a3d(3,1,3), msg)
    call assertEqual(6.0e19_REAL8  , a3d(1,2,3), msg)
    call assertEqual(-1.0e-23_REAL8, a3d(2,2,3), msg)
    call assertEqual(2.6_REAL8     , a3d(3,2,3), msg)

    ! A four dimensional real array. We also test FParser_size
    msg = trim(Package)//trim(Module)//" a4d, 4d real array, allocate mem"
    call FParser_size('a4d', 2, 1, 2, a4d_size)
    call assertEqual(2, a4d_size, msg)
    allocate(a4d(2,1,2,a4d_size))
    a4d = 1._REAL8
    call QueryFParser('a4d', a4d, .true., 2, 1, 2, a4d_size)
    call assertEqual(3.0_REAL8   , a4d(1,1,1,1), msg)
    call assertEqual(4.5_REAL8   , a4d(2,1,1,1), msg)
    call assertEqual(3.1_REAL8   , a4d(1,1,2,1), msg)
    call assertEqual(4.6_REAL8   , a4d(2,1,2,1), msg)
    call assertEqual(-3.4_REAL8  , a4d(1,1,1,2), msg)
    call assertEqual(4.7_REAL8   , a4d(2,1,1,2), msg)
    call assertEqual(5.2_REAL8   , a4d(1,1,2,2), msg)
    call assertEqual(4.6e19_REAL8, a4d(2,1,2,2), msg)

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Assumed starting dimension checks.
    ! We normally have
    !     asm1d(1) = 4. -7. 1.e19
    ! But we also allow
    !     asm1d = 4. -7. 1.e19
    ! Check that this actually works.
    msg = trim(Package)//trim(Module)//" asm1d, assume start dim"
    call FParser_size('asm1d', asm1d_size)
    call assertEqual(3, asm1d_size, msg)
    allocate(asm1d(asm1d_size))
    asm1d = 1.3_REAL8
    call QueryFParser('asm1d', asm1d, .true., asm1d_size)
    call assertEqual(4.0_REAL8  , asm1d(1), msg)
    call assertEqual(-7.0_REAL8 , asm1d(2), msg)
    call assertEqual(1.e19_REAL8, asm1d(3), msg)
    deallocate(asm1d)

    msg = trim(Package)//trim(Module)//" asm2d, assume start dim"
    call FParser_size('asm2d', 3, asm2d_size)
    call assertEqual(2, asm2d_size, msg)
    allocate(asm2d(3, asm2d_size))
    asm2d = 1.3_REAL8
    call QueryFParser('asm2d', asm2d, .true., 3, asm2d_size)
    call assertEqual(4.0_REAL8     , asm2d(1,1), msg)
    call assertEqual(-7.0_REAL8    , asm2d(2,1), msg)
    call assertEqual(1.e19_REAL8   , asm2d(3,1), msg)
    call assertEqual(-3.0_REAL8    , asm2d(1,2), msg)
    call assertEqual(1.9_REAL8     , asm2d(2,2), msg)
    call assertEqual(-2.3e-17_REAL8, asm2d(3,2), msg)
    deallocate(asm2d)

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Both dimensions for a 2d array are unknown.
    !
    ! Suppose we have the following input
    !       mults(1,1) = 0. 0.  1. 5.  6. 9.
    !       mults(1,2) = 3. 5.  8. 9.  10. 11. 20. 10
    !       mults(1,3) = 30. 5. 38. 3.
    ! In this case we don't know the size of either of the array dimensions, and
    ! of course the user does not know the size either and thus cannot somehow
    ! merge the above two lines.
    !
    ! The FParser_sizeb function is used to get both sizes so that memory allocation
    ! can be done. This size function should return 8 for the first dimension and
    ! 3 for the second dimension for the example above. The allocation would be
    !       allocate(mults(8,3))
    msg = trim(Package)//trim(Module)//" mults, both dimensions unknown"
    call FParser_sizeb('mults', mults_size1, mults_size2)
    call assertEqual(8, mults_size1, msg)
    call assertEqual(3, mults_size2, msg)
    allocate(mults(mults_size1, mults_size2))
    mults = 0._REAL8
    call QueryFParser('mults', mults, .true., mults_size1, mults_size2)
    call assertEqual(0._REAL8 , mults(1,1), msg)
    call assertEqual(0._REAL8 , mults(2,1), msg)
    call assertEqual(1._REAL8 , mults(3,1), msg)
    call assertEqual(5._REAL8 , mults(4,1), msg)
    call assertEqual(6._REAL8 , mults(5,1), msg)
    call assertEqual(9._REAL8 , mults(6,1), msg)

    call assertEqual(3._REAL8 , mults(1,2), msg)
    call assertEqual(5._REAL8 , mults(2,2), msg)
    call assertEqual(8._REAL8 , mults(3,2), msg)
    call assertEqual(9._REAL8 , mults(4,2), msg)
    call assertEqual(10._REAL8, mults(5,2), msg)
    call assertEqual(11._REAL8, mults(6,2), msg)
    call assertEqual(20._REAL8, mults(7,2), msg)
    call assertEqual(10._REAL8, mults(8,2), msg)

    call assertEqual(30._REAL8, mults(1,3), msg)
    call assertEqual(5._REAL8 , mults(2,3), msg)
    call assertEqual(38._REAL8, mults(3,3), msg)
    call assertEqual(3._REAL8 , mults(4,3), msg)

    deallocate(mults)

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------

    ! Another 1 dimension integer array. This tests the ability to use FParser_size
    ! to get the number of values first.
    ! The size function should return 6 for the following skip1d array, i.e.
    ! we need to allocate that much memory for the array. Before fixing a
    ! bug, the old, erroneous size function would return 4 (just count stuff
    ! after the = sign).
    msg = trim(Package)//trim(Module)//" skip1d, use FParser_size"
    call FParser_size('skip1d', skip1d_size)
    call assertEqual(6, skip1d_size, msg)
    allocate(skip1d(skip1d_size))
    skip1d = 5
    call QueryFParser('skip1d', skip1d, .true., skip1d_size)
    call assertEqual(1, skip1d(1), msg)
    call assertEqual(3, skip1d(2), msg)
    call assertEqual(5, skip1d(3), msg)
    call assertEqual(5, skip1d(4), msg)
    call assertEqual(2, skip1d(5), msg)
    call assertEqual(9, skip1d(6), msg)
    deallocate(skip1d)

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Character input.

    ! A single character without quotes
    call QueryFParser('single_char', single_char, .true.)
    call assertEqual('z', single_char, trim(Package)//trim(Module)//" single_char, single character")

    ! A single character with quotes
    call QueryFParser('single_charq', single_charq, .true.)
    call assertEqual('r', single_charq, trim(Package)//trim(Module)//" single_charq, single character in quotes")

    ! A simple character string.
    title = "zzzzzzzzzzzzzzzzzzzzzzzz"
    call QueryFParser('title', title, .true., 24)
    call assertEqual("Whatever the Title is.", title, trim(Package)//trim(Module)//" title, character string")

    ! A 1d array of character strings.
    msg = trim(Package)//trim(Module)//" char1d, 1d array of character strings"
    char1d = "zzzzzzzzzzzzzzzzzzzzzzzz"
    call QueryFParser('char1d', char1d, .true., 24, 6)
    call assertEqual("May"  , char1d(1), msg)
    call assertEqual("the"  , char1d(2), msg)
    call assertEqual("force", char1d(3), msg)
    call assertEqual("be"   , char1d(4), msg)
    call assertEqual("with" , char1d(5), msg)
    call assertEqual("you"  , char1d(6), msg)

    ! Test multiplicity for character strings.
    msg = trim(Package)//trim(Module)//" c1d_mult, multiplicity for character strings"
    c1d_mult = "zzzzzzzzzzzzzzzzzzzzzzzz"
    call QueryFParser('c1d_mult', c1d_mult, .true., 24, 5)
    call assertEqual("May"  , c1d_mult(1), msg)
    call assertEqual("the"  , c1d_mult(2), msg)
    call assertEqual("the"  , c1d_mult(3), msg)
    call assertEqual("the"  , c1d_mult(4), msg)
    call assertEqual("force", c1d_mult(5), msg)

    ! A 1d array of odd character strings.
    msg = trim(Package)//trim(Module)//" oddstrs, 1d array of odd character strings"
    call FParser_size('oddstrs', oddstrs_size)
    call assertEqual(7, oddstrs_size, msg)
    allocate(oddstrs(oddstrs_size))
    oddstrs = "zzzzzzzzzzzzzzzzzzzzzzzz"
    call QueryFParser('oddstrs', oddstrs, .true., 24, oddstrs_size)
    call assertEqual("123456789012345678901234", oddstrs(1), msg)
    call assertEqual(""                        , oddstrs(2), msg)
    call assertEqual("15"                      , oddstrs(3), msg)
    call assertEqual("14.35e19"                , oddstrs(4), msg)
    call assertEqual("123456789012345678901234", oddstrs(5), msg)
    call assertEqual("5"                       , oddstrs(6), msg)
    call assertEqual("a,b"                     , oddstrs(7), msg)

    ! A two dimensional character array. We test FParser_size and allocate memory.
    msg = trim(Package)//trim(Module)//" char2d, 2d character array"
    call FParser_size('char2d', 3, char2d_size)
    call assertEqual(2, char2d_size, msg)
    allocate(char2d(3,char2d_size))
    char2d = " "
    call QueryFParser('char2d', char2d, .true., 24, 3, char2d_size)
    call assertEqual("May"  , char2d(1,1), msg)
    call assertEqual("the"  , char2d(2,1), msg)
    call assertEqual("force", char2d(3,1), msg)
    call assertEqual("be"   , char2d(1,2), msg)
    call assertEqual("with" , char2d(2,2), msg)
    call assertEqual("you"  , char2d(3,2), msg)

    ! A three dimensional character array. We test FParser_size and allocate memory.
    msg = trim(Package)//trim(Module)//" char3d, 3d character array"
    call FParser_size('char3d', 2, 2, char3d_size)
    call assertEqual(2, char3d_size, msg)
    allocate(char3d(2,2,char3d_size))
    char3d = " "
    call QueryFParser('char3d', char3d, .true., 24, 2, 2, char3d_size)
    call assertEqual("a", char3d(1,1,1), msg)
    call assertEqual("b", char3d(2,1,1), msg)
    call assertEqual("c", char3d(1,2,1), msg)
    call assertEqual("d", char3d(2,2,1), msg)
    call assertEqual("e", char3d(1,1,2), msg)
    call assertEqual("f", char3d(2,1,2), msg)
    call assertEqual("g", char3d(1,2,2), msg)
    call assertEqual("h", char3d(2,2,2), msg)

    ! A four dimensional character array. We test FParser_size and allocate memory.
    msg = trim(Package)//trim(Module)//" char4d, 4d character array"
    call FParser_size('char4d', 2, 2, 2, char4d_size)
    call assertEqual(2, char4d_size, msg)
    allocate(char4d(2,2,2,char4d_size))
    char4d = " "
    call QueryFParser('char4d', char4d, .true., 24, 2, 2, 2, char4d_size)
    call assertEqual("a", char4d(1,1,1,1), msg)
    call assertEqual("b", char4d(2,1,1,1), msg)
    call assertEqual("c", char4d(1,2,1,1), msg)
    call assertEqual("d", char4d(2,2,1,1), msg)
    call assertEqual("e", char4d(1,1,2,1), msg)
    call assertEqual("f", char4d(2,1,2,1), msg)
    call assertEqual("g", char4d(1,2,2,1), msg)
    call assertEqual("h", char4d(2,2,2,1), msg)

    call assertEqual("i", char4d(1,1,1,2), msg)
    call assertEqual("j", char4d(2,1,1,2), msg)
    call assertEqual("k", char4d(1,2,1,2), msg)
    call assertEqual("l", char4d(2,2,1,2), msg)
    call assertEqual("m", char4d(1,1,2,2), msg)
    call assertEqual("n", char4d(2,1,2,2), msg)
    call assertEqual("o", char4d(1,2,2,2), msg)
    call assertEqual("p", char4d(2,2,2,2), msg)

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Restart block tests.
    code_varnames(1) = "rbtime"
    code_varnames(2) = "rbwhatever"
    write(code_values(1), '(e22.13)') 7.0_REAL8
    write(code_values(2), '(e22.13)') 3.0_REAL8
    code_vv_active(1) = 1
    code_vv_active(2) = 1
    rb_check = .false.
    call parser_get_rbnum(rb_num)
    if (.not.allocated(rb_triggered_indices)) then
       allocate(rb_triggered_indices(rb_num))
    endif
    rb_ntriggered = 0
    rb_triggered_indices = 0
    call FParser_rb_check(code_varnames, code_values, code_vv_active, &
         24, 2, rb_check, rb_ntriggered, rb_num, rb_triggered_indices)
    call assertTrue(rb_check, trim(Package)//trim(Module)//" restart_block, rb_check")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! When...then tests.
    shortmodcyc = 0
    modcyc = 0
    sim_time = 10.0_REAL8
    ncycle = 50
    wttf_c01 = .true.
    wt_cmd01 = .false.
    wt_cmd02 = 0._REAL8
    wttf_c02 = "The Force"
    wt_cmd03 = .false.
    wt_cmd04 = 0
    wt_cmd05 = 0

    code_varnames(1) = "time"
    code_varnames(2) = "ncycle"
    code_varnames(3) = "wttf_c01"
    code_varnames(4) = "wttf_c02"
    code_varnames(5) = "sim_pressure"
    write(code_values(1), '(e22.13)') sim_time
    write(code_values(2), '(i10)') ncycle
    if (wttf_c01) write(code_values(3), '(a)') "true"
    if (.not.wttf_c01) write(code_values(3), '(a)') "false"
    code_values(4) = wttf_c02
    write(code_values(5), '(e22.13)') -5.0e23
    code_vv_active(1) = 1
    code_vv_active(2) = 1
    code_vv_active(3) = 1
    code_vv_active(4) = 1
    code_vv_active(5) = 1

    call FParser_whenthen_num(wtnum)
    call assertEqual(11, wtnum, trim(Package)//trim(Module)//" when...then, wtnum")

    do i = 1, wtnum
       call FParser_whenthen_check(i, code_varnames, code_values, &
            code_vv_active, 24, 5, wt_check)
       if (wt_check) then
          call QueryFParser('shortmodcyc', shortmodcyc, .true.)
          call QueryFParser('modcyc', modcyc, .true.)
          call QueryFParser('wt_cmd01', wt_cmd01, .true.)
          call QueryFParser('wt_cmd02', wt_cmd02, .true.)
          call QueryFParser('wt_cmd03', wt_cmd03, .true.)
          call QueryFParser('wt_cmd04', wt_cmd04, .true.)
          call QueryFParser('wt_cmd05', wt_cmd05, .true.)
       endif
    enddo

    call FParser_whenthen_reset

    call assertEqual(5, shortmodcyc, trim(Package)//trim(Module)//" when...then, shortmodcyc")
    call assertEqual(5, modcyc, trim(Package)//trim(Module)//" when...then, modcyc")
    call assertTrue(wt_cmd01, trim(Package)//trim(Module)//" when...then, wt_cmd01")
    call assertEqual(5.0_REAL8, wt_cmd02, trim(Package)//trim(Module)//" when...then, wt_cmd02")
    call assertTrue(wt_cmd03, trim(Package)//trim(Module)//" when...then, wt_cmd03")
    call assertEqual(6, wt_cmd04, trim(Package)//trim(Module)//" when...then, wt_cmd04")
    call assertEqual(322, wt_cmd05, trim(Package)//trim(Module)//" when...then, wt_cmd05")

    ! The whenthen checks has already been done successfully above. Thus the
    ! check below should return false and shortmodcyc should end up being 2.
    shortmodcyc = 2
    do i = 1, wtnum
       call FParser_whenthen_check(i, code_varnames, code_values, &
            code_vv_active, 24,5,wt_check)
       if (wt_check) then
          call QueryFParser('shortmodcyc', shortmodcyc, .true.)
       endif
    enddo
    call FParser_whenthen_reset
    call assertEqual(2, shortmodcyc, trim(Package)//trim(Module)//" when...then, shortmodcyc, second chk")

    ! Check the when...then has gotten capability.
    num_wt_cyc = 5
    sim_pressure(1) = 0._REAL8
    sim_pressure(2) = 1.1_REAL8
    sim_pressure(3) = 10._REAL8
    sim_pressure(4) = 0.9_REAL8
    sim_pressure(5) = 0.5_REAL8

    do n = 1, num_wt_cyc
       write(code_values(5), '(e22.13)') sim_pressure(n)
       do i = 1, wtnum
          call FParser_whenthen_check(i, code_varnames, code_values, &
               code_vv_active, 24, 5, wt_check)
          if (wt_check) then
             call QueryFParser('wt_cmd06', wt_cmd06, .true.)
          endif
       enddo
    enddo
    call FParser_whenthen_reset

    call assertEqual(-400, wt_cmd06, trim(Package)//trim(Module)//" when...then, wt_cmd06")

    max_casize = 0
    do i = 1, wtnum
       call FParser_whenthen_casize(i, wt_casize)
       ! print *, wt_casize
       if (wt_casize .gt. max_casize) max_casize = wt_casize
    enddo
    call assertEqual(66, max_casize, trim(Package)//trim(Module)//" when...then, max_casize")
    ! print *, max_casize

    allocate(wt_ca(max_casize))

    do i = 1, wtnum
       wt_ca = " "
       call FParser_whenthen_casize(i, wt_casize)
       call FParser_whenthen_ca(i, wt_ca, wt_casize)
       !print *, wt_casize
       !do n = 1, wt_casize
       !   print *, wt_ca(n)
       !enddo
    enddo

    deallocate(wt_ca)

    max_satsize = 0
    do i = 1, wtnum
       call FParser_whenthen_satsize(i, wt_satsize)
       !print *, "&&&&&cw_sat test_parser.f90, wt_satsize = ", wt_satsize
       if (wt_satsize .gt. max_satsize) max_satsize = wt_satsize
    enddo
    call assertEqual(2, max_satsize, trim(Package)//trim(Module)//" when...then, max_satsize")
    !print *, "&&&&&cw_sat test_parser.f90, max_satsize = ", max_satsize

    if (max_satsize .gt. 0) then
       allocate(wt_sat(max_satsize))

       do i = 1, wtnum
          wt_sat = 0
          call FParser_whenthen_satsize(i, wt_satsize)
          call FParser_whenthen_getsat(i, wt_sat, wt_satsize)
          if (i.eq.6) &
               call assertEqual(1, wt_sat(1), trim(Package)//trim(Module)//" when...then, getsat")
          !print *, "&&&&&cw_sat test_parser.f90, in getsat loop, wt_satsize = ", wt_satsize
          !do n = 1, wt_satsize
          !   print *, "&&&&&cw_sat test_parser.f90, in getsat loop, wt_sat = ", wt_sat(n)
          !enddo
       enddo

       deallocate(wt_sat)
    endif

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! If statement tests.
    delta_y_cmd01 = 0._REAL8
    call QueryFParser('delta_y_cmd01', delta_y_cmd01, .true.)
    call assertEqual(1.0_REAL8, delta_y_cmd01, trim(Package)//trim(Module)//" single line if, delta_y_cmd01")

    delta_y_cmd02 = 0._REAL8
    call QueryFParser('delta_y_cmd02', delta_y_cmd02, .true.)
    call assertEqual(1.0_REAL8, delta_y_cmd02, trim(Package)//trim(Module)//" single line if with var, delta_y_cmd02")

    delta_y_cmd03 = 0._REAL8
    call QueryFParser('delta_y_cmd03', delta_y_cmd03, .true.)
    call assertEqual(3.0_REAL8, delta_y_cmd03, trim(Package)//trim(Module)//" single line if with var, delta_y_cmd03")

    delta_y_cmd04 = 0
    call QueryFParser('delta_y_cmd04', delta_y_cmd04, .true.)
    call assertEqual(3, delta_y_cmd04, trim(Package)//trim(Module)//" single line if, var array, delta_y_cmd04")

    delta_y_cmd05 = 0._REAL8
    call QueryFParser('delta_y_cmd05', delta_y_cmd05, .true.)
    call assertEqual(0.1_REAL8, delta_y_cmd05, trim(Package)//trim(Module)//" block if with var, delta_y_cmd05")

    delta_y_cmd06 = 0._REAL8
    call QueryFParser('delta_y_cmd06', delta_y_cmd06, .true.)
    call assertEqual(0.2_REAL8, delta_y_cmd06, trim(Package)//trim(Module)//" nested if with var, delta_y_cmd06")

    delta_y_cmd07 = 0._REAL8
    call QueryFParser('delta_y_cmd07', delta_y_cmd07, .true.)
    call assertEqual(0.26_REAL8, delta_y_cmd07, trim(Package)//trim(Module)//" if/else/endif, delta_y_cmd07")

    delta_x_cmd01 = 0._REAL8
    call QueryFParser('delta_x_cmd01', delta_x_cmd01, .true.)
    call assertEqual(1.48_REAL8, delta_x_cmd01, trim(Package)//trim(Module)//" if/elseif/else/endif, delta_x_cmd01")

    delta_x_cmd02 = 0._REAL8
    call QueryFParser('delta_x_cmd02', delta_x_cmd02, .true.)
    call assertEqual(43.56_REAL8, delta_x_cmd02, trim(Package)//trim(Module)//" if/elseif/else/endif, delta_x_cmd02")

    delta_x_cmd03 = 0._REAL8
    call QueryFParser('delta_x_cmd03', delta_x_cmd03, .true.)
    call assertEqual(-3.4_REAL8, delta_x_cmd03, trim(Package)//trim(Module)//" if/elseif/else/endif, delta_x_cmd03")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Do loop tests
    do_sum_cmd01 = 0
    call QueryFParser('do_sum_cmd01', do_sum_cmd01, .true.)
    call assertEqual(10, do_sum_cmd01, trim(Package)//trim(Module)//" simple do loop, do_sum_cmd01")

    do_sum_cmd02 = 0
    call QueryFParser('do_sum_cmd02', do_sum_cmd02, .true.)
    call assertEqual(1000, do_sum_cmd02, trim(Package)//trim(Module)//" nested do loop, do_sum_cmd02")

    do_sum_cmd03 = 0
    call QueryFParser('do_sum_cmd03', do_sum_cmd03, .true.)
    call assertEqual(200, do_sum_cmd03, trim(Package)//trim(Module)//" do loop exit, do_sum_cmd03")

    do_sum_cmd04 = 0
    call QueryFParser('do_sum_cmd04', do_sum_cmd04, .true.)
    call assertEqual(900, do_sum_cmd04, trim(Package)//trim(Module)//" do loop cycle, do_sum_cmd04")

    do_sum_cmd05 = 0
    call QueryFParser('do_sum_cmd05', do_sum_cmd05, .true.)
    call assertEqual(190, do_sum_cmd05, trim(Package)//trim(Module)//" do loop math eval, do_sum_cmd05")

    do_sum_cmd06 = 0
    call QueryFParser('do_sum_cmd06', do_sum_cmd06, .true.)
    call assertEqual(40, do_sum_cmd06, trim(Package)//trim(Module)//" do loop step, do_sum_cmd06")

    do_sum_cmd07 = 0
    call QueryFParser('do_sum_cmd07', do_sum_cmd07, .true.)
    call assertEqual(7, do_sum_cmd07, trim(Package)//trim(Module)//" do loop negative step, do_sum_cmd07")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! File include tests.
    inc_cmd01 = ZERO
    call QueryFParser('inc_cmd01', inc_cmd01, .true.)
    call assertEqual(3.0_REAL64, inc_cmd01, trim(Package)//trim(Module)//" simple include, inc_cmd01")

    inc_cmd02 = ZERO
    call QueryFParser('inc_cmd02', inc_cmd02, .true.)
    call assertEqual(5.0_REAL64, inc_cmd02, trim(Package)//trim(Module)//" nested include, inc_cmd02")

    inc_cmd03 = ZERO
    call QueryFParser('inc_cmd03', inc_cmd03, .true.)
    call assertEqual(7.0_REAL64, inc_cmd03, trim(Package)//trim(Module)//" nested include, inc_cmd03")

    inc_cmd04 = ZERO
    call QueryFParser('inc_cmd04', inc_cmd04, .true.)
    call assertEqual(-19.0_REAL64, inc_cmd04, trim(Package)//trim(Module)//" nested include, inc_cmd04")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Subroutine tests
    sub_cmd01 = 0
    call QueryFParser('sub_cmd01', sub_cmd01, .true.)
    call assertEqual(3, sub_cmd01, trim(Package)//trim(Module)//" simple sub call, no args, sub_cmd01")

    sub_cmd02 = 0
    call QueryFParser('sub_cmd02', sub_cmd02, .true.)
    call assertEqual(9, sub_cmd02, trim(Package)//trim(Module)//" nested sub calls, no args, sub_cmd02")

    sub_cmd03 = 0
    call QueryFParser('sub_cmd03', sub_cmd03, .true.)
    call assertEqual(3, sub_cmd03, trim(Package)//trim(Module)//" sub args, sub_cmd03")

    sub_cmd04 = 0
    call QueryFParser('sub_cmd04', sub_cmd04, .true.)
    call assertEqual(4, sub_cmd04, trim(Package)//trim(Module)//" sub args, sub_cmd04")

    sub_cmd05 = 0
    call QueryFParser('sub_cmd05', sub_cmd05, .true.)
    call assertEqual(52, sub_cmd05, trim(Package)//trim(Module)//" sub args, sub_cmd05")

    sub_cmd06 = 0
    msg = trim(Package)//trim(Module)//" sub args array, sub_cmd06"
    call QueryFParser('sub_cmd06', sub_cmd06, .true., 4)
    call assertEqual(300, sub_cmd06(1), msg)
    call assertEqual(16 , sub_cmd06(2), msg)
    call assertEqual(7  , sub_cmd06(3), msg)
    call assertEqual(300, sub_cmd06(4), msg)

    sub_cmd07 = 0
    call QueryFParser('sub_cmd07', sub_cmd07, .true.)
    call assertEqual(300, sub_cmd07, trim(Package)//trim(Module)//" sub args, sub_cmd07")

    sub_cmd08 = 0
    call QueryFParser('sub_cmd08', sub_cmd08, .true.)
    call assertEqual(52, sub_cmd08, trim(Package)//trim(Module)//" sub args, sub_cmd08")

    sub_cmd09 = 0
    call QueryFParser('sub_cmd09', sub_cmd09, .true.)
    call assertEqual(3, sub_cmd09, trim(Package)//trim(Module)//" sub args, sub_cmd09")

    sub_cmd10 = 0
    call QueryFParser('sub_cmd10', sub_cmd10, .true.)
    call assertEqual(6, sub_cmd10, trim(Package)//trim(Module)//" sub args ret, sub_cmd10")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Example 2 in the manual.
    nummat = 0
    call QueryFParser('nummat', nummat, .true.)
    call assertEqual(3, nummat, trim(Package)//trim(Module)//" Example 2, nummat")

    numreg = 0
    call QueryFParser('numreg', numreg, .true.)
    call assertEqual(3, numreg, trim(Package)//trim(Module)//" Example 2, numreg")

    msg = trim(Package)//trim(Module)//" Example 2, matdef"
    matdef = 0._REAL8
    call QueryFParser('matdef', matdef, .true., 200, 99)

    call assertEqual(0.40_REAL8  , matdef(16,1), msg)
    call assertEqual(1.0e13_REAL8, matdef(30,1), msg)
    call assertEqual(0.67_REAL8  , matdef(16,2), msg)
    call assertEqual(1.0e13_REAL8, matdef(30,2), msg)
    call assertEqual(0.40_REAL8  , matdef(16,3), msg)
    call assertEqual(3.0e13_REAL8, matdef(30,3), msg)
    call assertEqual(0.40_REAL8  , matdef(17,1), msg)
    call assertEqual(1.0e13_REAL8, matdef(31,1), msg)
    call assertEqual(0.67_REAL8  , matdef(17,2), msg)
    call assertEqual(1.0e13_REAL8, matdef(31,2), msg)
    call assertEqual(0.40_REAL8  , matdef(17,3), msg)
    call assertEqual(3.0e13_REAL8, matdef(31,3), msg)

    msg = trim(Package)//trim(Module)//" Example 2, matreg"
    matreg = 0
    call QueryFParser('matreg', matreg, .true., 99)

    call assertEqual(1, matreg(1), msg)
    call assertEqual(2, matreg(2), msg)
    call assertEqual(3, matreg(3), msg)

    msg = trim(Package)//trim(Module)//" Example 2, matreg2"
    matreg2 = 0
    call QueryFParser('matreg2', matreg2, .true., 99)
    call assertEqual(1, matreg2(1), msg)
    call assertEqual(2, matreg2(2), msg)
    call assertEqual(3, matreg2(3), msg)

    numreg2 = 0
    call QueryFParser('numreg2', numreg2, .true.)
    call assertEqual(3, numreg2, trim(Package)//trim(Module)//" Example 2, numreg2")

    ! ---------------------------------------------------------------------------
    ! ---------------------------------------------------------------------------
    ! Quadratic Root Example
    quad_root1 = 0
    call QueryFParser('quad_root1', quad_root1, .true.)
    call assertEqual(3, quad_root1, trim(Package)//trim(Module)//" Quadratic eqn, root1")

    quad_root2 = 0
    call QueryFParser('quad_root2', quad_root2, .true.)
    call assertEqual(-5, quad_root2, trim(Package)//trim(Module)//" Quadratic eqn, root2")

   ! ******************************************************************************
   ! ******************************************************************************
   ! Parser - Check Processed
   ! At this point, all the parsing has been done, check that everything has been
   ! processed. We could also terminate the parser, i.e. free memory, clean up.
   ! ******************************************************************************
   ! ******************************************************************************
   !call FParser_terminate(good)

   call parser_comm_destroy
   call parser_destroy

   ! ******************************************************************************
   ! ******************************************************************************
   ! Parser - Final Summary
   ! ******************************************************************************
   ! ******************************************************************************

   ! Close the output file.
   close(12)

end program FParserTest

