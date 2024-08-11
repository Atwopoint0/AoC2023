program aoc06a
    implicit none

    integer :: i, j
    integer, dimension(4) :: times, distances, record   
    integer :: t, r
    real :: d
 
    !part 1
    times = [51, 92, 68, 90]
    distances = [222, 2031, 1126, 1225]
    record = 0
 
    do i = 1, 4
        if (mod(times(i),2) == 0) then
            if ((times(i)/2)*times(i)/2 - distances(i) > 0) then
                    record(i) = record(i) + 1
            end if
            
            do j = (times(i)+2)/2, times(i)
                if ((times(i)-j)*j - distances(i) > 0) then
                    record(i) = record(i) + 2
                else 
                    exit
                end if
            end do
            
        else if (mod(times(i),2) == 1) then
            do j = (times(i)+1)/2, times(i)
                if ((times(i)-j)*j - distances(i) > 0) then
                    record(i) = record(i) + 2
                else 
                    exit
                end if
            end do
        endif
    end do
 
    print*, product(record)
 
end program aoc06a