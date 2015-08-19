PROGRAM xsort
C driver for routine sort
C print original array
    write(*,*) 'Original array'
C sort array
C print sorted array
    write(*,*) 'Sorted array'
    write(*,*) '...and array B is:'
    do 13 i=1,10
      write(*,'(1x,10f7.2)') (b(10*(i-1)+j), j=1,10
13  continue
      write(*,*) 'press RETURN to continue...'
read(*,*)
C sort B and mix A
