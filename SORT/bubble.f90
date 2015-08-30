INTEGER :: i, j, x,temp, y
INTEGER :: a(100)
PRINT*, "ENTER THE SIZE OF THE ARRAY"
READ*, x
PRINT*, "SIZE OF THE ARRAY IS ",x
PRINT*, "ENTER THE ELEMENTS OF THE ARRAY"
DO i=1, x, 1
            READ*,a(i)
END DO
PRINT*, "THE ARRAY IS AS FOLLOW:"
DO i=1, x, 1
            PRINT*, a(i)
END DO
PRINT*, "PRESS 1 TO SORT THE ARRAY IN ASCENDING ORDER"
PRINT*, "PRESS 2 TO SORT THE ARRAY IN DESCENDING ORDER"
READ*, y
IF (y = = 1) THEN
            DO i =1, x-1, 1
                        DO j =1, x-1, 1
                                    IF (a(j)>a(j+1)) THEN
                                                temp = a(j)
                                                a(j) = a(j+1)
                                                a(j+1) = temp
                                    END IF
                        END DO
            END DO
END IF
IF (y = = 2) THEN
            DO i =1, x-1, 1
                        DO j=1, x-1, 1
                                    if (a(j)<a(j+1)) THEN
                                                temp = a(j)
                                                a(j) = a(j+1)
                                                a(j+1) = temp
                                    END IF
                        END DO
            END DO
END IF
PRINT*, "AFTER SORTING"
DO j=1, x, 1
            PRINT*, a(j)
END DO
STOP
END
