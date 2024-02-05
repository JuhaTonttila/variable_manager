MODULE mo_parameters
    IMPLICIT NONE

    ! For character lengthts...need to use these to get around some allocation troubles with GCC 9.4.0
    INTEGER, PARAMETER :: ch_short = 20,   &
                          ch_long = 200

    INTEGER, PARAMETER :: nxp = 5, nyp = 4, nzp = 3

END MODULE mo_parameters