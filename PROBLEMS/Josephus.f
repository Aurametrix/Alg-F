program josephus
   implicit none
   integer :: n, i, k, p
   integer, allocatable :: next(:)
   read *, n, k
   allocate(next(0:n - 1))
   do i = 0, n - 2
      next(i) = i + 1
   end do
   next(n - 1) = 0
   p = 0
   do while(next(p) /= p)
      do i = 1, k - 2
         p = next(p)
      end do
      print *, "Kill", next(p)
      next(p) = next(next(p))
      p = next(p)
   end do
   print *, "Alive", p
   deallocate(next)
end program

! friendly interactive shell
!
function execute
    # If the list is empty, don't do anything.
    test (count $argv) -ge 2; or return
    # If the list has only one element, return it
    if test (count $argv) -eq 2
        echo $argv[2]
        return
    end
    # Rotate prisoners
    for i in (seq 2 $argv[1])
        set argv $argv[1 3..-1 2]
    end
    # Mention killed prisoner
    echo $argv[2]
    # Kill rest recursively
    execute $argv[1 3..-1]
end
 
echo Prisoner (execute 3 (seq 0 40))[-1] survived.
echo Prisoners (execute 3 (seq 0 40))[-3..-1] survived.
echo Prisoner (execute 2 Joe Jack William Averell Rantanplan)[-1] survived.
