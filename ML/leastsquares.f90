!*********************************************************
!*    Approximation of a discreet real function F(x) by  *
!*    least squares                                      *
!* ----------------------------------------------------- *
!* Ref.: "Méthodes de calcul numérique, Tome 2 By Claude *
!*        Nowakowski, PSI Edition, 1984" [BIBLI 04].     *
!* ----------------------------------------------------- *
!* SAMPLE RUN:                                           *
!*                                                       *
!* Number of points    : 11                              *
!*                                                       *
!* Degree of polynomial: 3                               *
!*                                                       *
!* Function to approximate:                              *
!*  X(1), Y(1) = 0 0                                     *
!*  X(2), Y(2) = 0.1 0.2                                 *
!*  X(3), Y(3) = 0.2 0.4                                 *
!*  X(4), Y(4) = 0.3 0.6                                 *
!*  X(5), Y(5) = 0.4 0.8                                 *
!*  X(6), Y(6) = 0.5 1                                   *
!*  X(7), Y(7) = 0.6 0.8                                 *
!*  X(8), Y(8) = 0.7 0.6                                 *
!*  X(9), Y(9) = 0.8 0.4                                 *
!*  X(10), Y(10) = 0.9 0.2                               *
!*  X(11), Y(11) = 1 0                                   *
!*                                                       *
!* Polynomial approximation of degree 3 (11 points)      *
!* Coefficients of polynomial:                           *
!*  A(0) =    -0.069930070                               *
!*  A(1) =     3.496503497                               *
!*  A(2) =    -3.496503497                               *                      
!*  A(3) =     0.000000000                               *
!*                                                       *
!* Approximated function:                                *
!*        X           Y                                  *
!*    0.000000   -0.069930                               *
!*    0.100000    0.244755                               *
!*    0.200000    0.489510                               *
!*    0.300000    0.664336                               *
!*    0.400000    0.769231                               *
!*    0.500000    0.804196                               *
!*    0.600000    0.769231                               *
!*    0.700000    0.664336                               *
!*    0.800000    0.489510                               *
!*    0.900000    0.244755                               *
!*    1.000000   -0.069930                               *
!*                                                       *
!*                    F90 Version By J-P Moreau, Paris.  *
!*                           (www.jpmoreau.fr)           *
!*********************************************************
Program Approx

parameter(SIZE=25)

integer i,ij,j,k,n,n1,m,m1,m2
real*8  C(SIZE,SIZE)
real*8  A(SIZE),B(SIZE),X(SIZE),Xc(SIZE),Y(SIZE),Yx(SIZE)
real*8  p,xx,s,yc

  write(*,10,advance='no'); read *, n
  n=n-1
  write(*,20,advance='no'); read *, m
  n1=n+1; m1=m+1; m2=m+2
  print *,' '
  print *,' Function to approximate:'
  do i=1, n1
	if (i<10) then 
	  write(*,30,advance='no') i, i
    else
	  write(*,31,advance='no') i, i
    end if
	read *, X(i), Y(i)
  end do
  do k=1, m2
    Xc(k)=0.d0
    do i=1, n1
	  Xc(k) = Xc(k) + X(i)**k
    end do
  end do
  yc=0.d0
  do i=1, n1  
    yc = yc + Y(i)
  end do
  do k=1, m
    Yx(k)=0.d0
    do i=1, n1
	  Yx(k) =  Yx(k) + Y(i)*X(i)**k
    end do 
  end do
  do i=1, m1
	do j=1, m1
      ij=i+j-2
      if (i==1.and.j==1)  then
	    C(1,1) = n1
      else 
	    C(i,j)=Xc(ij)
      end if 
    end do
  end do
  B(1)=yc; 
  do i=2,m1
    B(i)=Yx(i-1)
  end do 
  do k=1, m
    do i=k+1, m1
      B(i) = B(i) - C(i,k)/C(k,k)*B(k)
      do j=k+1, m1
        C(i,j) = C(i,j) - C(i,k)/C(k,k)*C(k,j)
      end do
    end do
  end do
  A(m1)=B(m1)/C(m1,m1)
  do i=m, 1, -1
    s=0.d0
    do k=i+1, m1  
	  s = s + C(i,k)*A(k)
    end do 
    A(i) = (B(i)-s)/C(i,i)
  end do
  print *,' '
  write(*,40)  m, n+1
  print *,' Coefficients of polynomial:'
  do i=1, m1
    write(*,50)  i-1, A(i)
  end do
  print *,' ' 
  print *,' Approximated function:'
  print *,'       X           Y  '
  do i=1, n1
    xx=X(i); p=0.d0
    do k=1, m1
	  p = p*xx + A(m1+1-k)
    end do 
    write(*,60)  xx, p
  end do
  print *,' '
  print *,' '

10 format(/'  Number of points    : ')
20 format(/'  Degree of polynomial: ')
30 format('   X(',I1,'), Y(',I1,') = ')
31 format('   X(',I2,'), Y(',I2,') = ')

40 format('  Polynomial approximation of degree ',I2,' (',I2,' points)'/)
50 format('    A(',I,') = ',F15.9)
60 format(2F12.6)

end

! end of file approx.f90
