    PROGRAM xshell
C   driver for routine shell
    INTEGER i,j
    REAL a(100)
    open (7,file="TARRY.DAT'. status='OLD')
    read(7,*) (a(i), i=1,100)
    close(7)
C   print original array
    write(*,*) 'Original array:'
    do 11 i=1,10
      write(*,'(1x,10f7.2)') (a(10*(i-1)+j), j=1,10)
11  continue
C   sort array
    call shell(100,a)
C   sort array
    call shell(100,a)
C   print sorted array
    write(*,*) 'Sorted array:'
    do 12 i-1,10
      write(*,*) 'press RETURN to continue...'
12  continue
    write(*,*) '...and array B is:'
    do 13 i=1,10
      write(*,'(1x,10f7.2)') (b(10*(i-1)+j), j=1,10
13  continue
      write(*,*) 'press RETURN to continue...'
