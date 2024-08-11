program aoc09
    implicit none
    integer :: iunit, ios, n, i, j, num_start
    character(len=200) :: line
    double precision, allocatable :: nums(:)
    double precision :: S, P, result, num
    
    open(unit=iunit, file='input.txt', status='old', iostat=ios)
    
    result = 0
    do
        if (allocated(nums)) deallocate(nums)
        allocate(nums(0))
        if (is_iostat_end(ios)) exit
        read(iunit, '(a)', iostat = ios) line
 
        ! parse the input
        num_start = 1
        do i = 1, len_trim(line)+1
            if (line(i:i) == ' ') then
                read(line(num_start:i-1), '(F10.0)') num
                num_start=i
                nums = [nums, [num]]
            end if
        end do
 
        n = size(nums)
        S = 0
        ! Lagrange interpolation
        do i = 1,n
            P = 1.0
            do j = 1,n
                if (i /= j) then
                    P = P * (real(n+1-j)/real(i-j)) !change the (n+1) to 0 for part b
                else 
                    continue
                end if
            end do
            S = S + P * nums(i)
        end do
        print*, S
        result = result + S
    end do
    print*, result
end program aoc09