subroutine update2(jpass)

  !================================================================
  ! Subroutine to update time array
  !================================================================

  use part_def, only : tzero, time, dt

  implicit none


  integer, intent(in) :: jpass

  integer :: j, isec, imin, ihour, iday, imon, iyear, kd, ishift

  real*8 :: isecnew, iminnew, ihournew ! CRITICAL: problem with floor function otherwise


  j = jpass

  iday = tzero(4)

  imon = tzero(5)

  iyear = tzero(6)

  call cday2(iday,imon,iyear,kd)


  ! Enables computation backward in time
#ifdef BACK
  isecnew = tzero(1) - dt*j
#else
  isecnew = tzero(1) + dt*j
#endif

  isec = modulo(int(isecnew),60)

  iminnew = tzero(2) + floor(isecnew/60)

  imin = modulo(int(iminnew),60)

  ihournew =  tzero(3) + floor(iminnew/60)

  ihour  = modulo(int(ihournew),24)

  kd = kd + floor(ihournew/24)


  call dmy2(iday,imon,iyear,kd)


  time(1) = isec

  time(2) = imin

  time(3) = ihour

  time(4) = iday

  time(5) = imon

  time(6) = iyear


  return

end subroutine update2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine cday2(idd,imm,iyy_pass,kd)

  !=====================================================================
  !
  ! Given day, month (each 2 digits) and year (4 digits), 
  ! cday returns the day#, kd, based on the gregorian calendar.
  ! The gregorian calendar, currently 'universally' in use, 
  ! was initiated in europe in the sixteenth century. 
  ! Note that cday is valid only for gregorian calendar dates.
  !
  ! kd = 1 corresponds to january 1, 0000
  !       
  ! Note that the gregorian reform of the julian calendar omitted 10 days
  ! in 1582 in order to restore the date of the vernal equinox to march 21 
  ! (the day after oct 4, 1582 became oct 15, 1582), and revised the leap 
  ! year rule so that centurial years not divisible by 400 were not leap years.
  !
  ! This routine was written by Eugene Neufeld, at ios, in june 1990.
  ! Modified by Joel Chasse (January 20th, 1995)
  !
  !=====================================================================

  implicit none

  integer, intent(in out) :: idd, imm, iyy_pass

  integer, intent(out) :: kd

  integer :: i, icc, iyy, jcc, jfh, jfy, jyy, jyyy, kk, kkd, l, lp

  integer, dimension(13) :: ndp

  integer, dimension(12) :: ndm


  data ndp /0,31,59,90,120,151,181,212,243,273,304,334,365/

  data ndm /31,28,31,30,31,30,31,31,30,31,30,31/


  icc = int(iyy_pass/100.)

  iyy = iyy_pass-icc*100

  lp = 6

  ! Test for invalid input:

1 format(' input error. icc = ',i7)

2 format(' input error. iyy = ',i7)

3 format(' input error. imm = ',i7)

4 format(' input error. idd = ',i7)

  if(icc<0)then
     write(lp,1) icc
     stop
  endif

  if(iyy<0.or.iyy>99)then
     write(lp,2) iyy
     stop
  endif

  if(imm<=0.or.imm>12)then
     write(lp,3) imm
     stop
  endif

  if(idd<=0)then
     write(lp,4) idd
     stop
  endif

  if(imm/=2.and.idd>ndm(imm))then
     write(lp,4) idd
     stop
  endif

  if(imm==2.and.idd>29)then
     write(lp,4) idd
     stop
  endif

  if(imm==2.and.idd>28.and.((iyy/4)*4-iyy/=0.or. &
     (iyy==0.and.(icc/4)*4-icc/=0))) then
     write(lp,4) idd
     stop
  endif

  ! Calculate day# of last day of last century:
  kd = icc*36524 + (icc+3)/4

  ! Calculate day# of last day of last year:
  kd = kd + iyy*365 + (iyy+3)/4

  ! Adjust for century rule:
  !  viz. no leap-years on centurys except when 
  !  the 2-digit century is divisible by 4.
  if(iyy>0.and.(icc-(icc/4)*4)/=0) kd = kd-1

  ! kd now truly represents the day# of the last day of last year.

  ! Calculate day# of last day of last month:
  kd = kd + ndp(imm)

  ! Adjust for leap years:
  if(imm>2.and.((iyy/4)*4-iyy)==0.and.((iyy/=0) &
     .or.(((icc/4)*4-icc)==0))) kd = kd+1

  ! kd now truly represents the day# of the last day of the last month.

  ! Calculate the current day#:
  kd = kd + idd

  return

!-----------------------------------------------------------------------

  entry dmy2(idd,imm,iyy_pass,kd)

  !=====================================================================
  !
  ! Given the (gregorian) day#, kd, as calculated above in this routine,
  ! entry dmy2 returns the (gregorian) day, month, year and century.
  !
  !=====================================================================

  ! Test for valid input:

5 format(' kd = ',i7,' invalid input. dmy2 stop.')

  if(kd<=0) write(lp,5) kd

  ! Save kd
  kkd = kd

  ! Calculate icc and subtract the number of days represented by icc from kkd
  ! jfh is the number of 400 year intervals up to kkd
  ! jcc is the number of additional centuries up to kkd

  jfh = kkd/146097

  kkd = kkd - jfh*146097

  if(kkd<36525)then

     jcc = 0

  else

     kkd = kkd - 36525

     jcc = 1 + kkd/36524

     kkd = kkd - (jcc-1)*36524

  endif

  icc = 4*jfh + jcc

  if(kkd==0)then

     icc = icc-1

     iyy = 99

     imm = 12

     idd = 31

     iyy_pass = icc*100+iyy

     return

  endif

  ! Calculate iyy. jfy is the number of four year intervals in the current century.
  ! The first four year interval is short (1460 days rather than 1461) if 
  ! the current century is not divisible by 4, and in this case jcc.ne.0 as calculated above.

  ! Calculate jfy:

  jfy = 0

  if(jcc==0) goto 10

  if(kkd<1460) goto 10

  jfy = 1

  kkd = kkd - 1460

10 kk = kkd/1461

  jfy = jfy + kk

  kkd = kkd - kk*1461

  ! Calculate jyy, the remaining years of the current century up to the current day:

  jyy = 0

  ! The next year is not a leap year if jfy=0 and jcc.ne.0.

  if(jfy==0.and.jcc/=0) goto 20

  if(kkd<366) goto 30

  jyy = 1

  kkd = kkd - 366

20 jyyy = kkd/365

  jyy = jyy + jyyy

  kkd = kkd - jyyy*365

30 iyy = 4*jfy + jyy

  if(kkd==0) then

     iyy = iyy-1

     imm = 12

     idd = 31

     iyy_pass = icc*100+iyy

     return

  endif

  ! Set l=1 if we have a leap year.

  l = 0

  if(iyy-(iyy/4)*4/=0) goto 40

  if(iyy==0.and.(icc-(icc/4)*4)/=0) goto 40

  l = 1

  ! Calculate imm and idd
40 if(kkd>31) goto 50

  imm = 1

  idd = kkd

  iyy_pass = icc*100+iyy

  return

50 if(kkd>59) goto 60

  imm = 2

  idd = kkd-31

  iyy_pass = icc*100+iyy

  return

60 if(kkd>60) goto 70

  if(l==0) goto 70

  imm = 2

  idd = 29

  iyy_pass = icc*100+iyy

  return

70 if(l==1) kkd = kkd-1

  do i = 4,13

     if(kkd>ndp(i)) cycle

     imm = i-1

     idd = kkd - ndp(i-1)

     iyy_pass = icc*100+iyy

     return

  enddo

6 format(' error in dmy2')

  write(lp,6)

  stop

end subroutine cday2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
