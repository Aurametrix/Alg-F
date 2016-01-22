!**********************************************************************************************************************************
!
!                                                          L I N R E G
!
!  Program:      LINREG
!
!  Programmer:   Dr. David G. Simpson
!                Department of Physical Science
!                Prince George's Community College
!                Largo, Maryland  20774
!
!  Date:         January 21, 2002
!
!  Language:     Fortran-90
!
!  Description:  This program performs a linear regression analysis for a set of data given as (x,y) pairs.  The output from
!                the program is the slope and y-intercept of the least-squares best fit straight line through the data points.
!
!**********************************************************************************************************************************


!**********************************************************************************************************************************
!  Main program
!**********************************************************************************************************************************

program linreg

   implicit none                                                                    ! no default data types

   integer, parameter  :: dbl = kind (0.0d0)                                        ! define kind for double precision

   real(dbl)           ::  b                                                        ! y-intercept of least-squares best fit line
   real(dbl)           ::  m                                                        ! slope of least-squares best fit line
   real(dbl)           ::  n = 0.0d0                                                ! number of data points
   real(dbl)           ::  r                                                        ! squared correlation coefficient
   character (len=80)  ::  str                                                      ! input string
   real(dbl)           ::  sumx  = 0.0d0                                            ! sum of x
   real(dbl)           ::  sumx2 = 0.0d0                                            ! sum of x**2
   real(dbl)           ::  sumxy = 0.0d0                                            ! sum of x * y
   real(dbl)           ::  sumy  = 0.0d0                                            ! sum of y
   real(dbl)           ::  sumy2 = 0.0d0                                            ! sum of y**2
   real(dbl)           ::  x                                                        ! input x data
   real(dbl)           ::  y                                                        ! input y data

   write (unit=*, fmt="(a)") " LINREG - Perform linear regression"                  ! print introductory message
   write (unit=*, fmt="(a/)") "   (Enter END to stop data entry and compute"//  &
                              " linear regression.)"

   do                                                                               ! loop for all data points
      write (unit=*, fmt="(a)", advance="no") " Enter x y:  "                       ! prompt to enter data
      read (unit=*, fmt="(a)") str                                                  ! read x and y into string
      if (str == "end" .or. str == "END") exit                                      ! if no more data, then exit loop
      read (unit=str, fmt=*) x, y                                                   ! else read x and y from string

      n = n + 1.0d0                                                                 ! increment number of data points by 1
      sumx  = sumx + x                                                              ! compute sum of x
      sumx2 = sumx2 + x * x                                                         ! compute sum of x**2
      sumxy = sumxy + x * y                                                         ! compute sum of x * y
      sumy  = sumy + y                                                              ! compute sum of y
      sumy2 = sumy2 + y * y                                                         ! compute sum of y**2
   end do

   m = (n * sumxy  -  sumx * sumy) / (n * sumx2 - sumx**2)                          ! compute slope
   b = (sumy * sumx2  -  sumx * sumxy) / (n * sumx2  -  sumx**2)                    ! compute y-intercept
   r = (sumxy - sumx * sumy / n) /                                     &            ! compute correlation coefficient
                     sqrt((sumx2 - sumx**2/n) * (sumy2 - sumy**2/n))

   write (unit=*, fmt="(/a,es15.6)") " Slope        m = ", m                        ! print results
   write (unit=*, fmt="(a, es15.6)") " y-intercept  b = ", b
   write (unit=*, fmt="(a, es15.6)") " Correlation  r = ", r

end program linreg
