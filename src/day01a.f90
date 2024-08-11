program aoc01a
    implicit none
   
    integer :: status, i, linesum, result, iunit
    character(len=999) :: line
    character(len=999) :: lineNums
    character(len=1), dimension(9) :: nums = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
 
    status = 0
    result = 0
   
    open(newunit=iunit, file='input.txt', status='OLD')
    do while (status == 0)
        linesum = 0
        lineNums = ''
 
        read (iunit, '(a)', iostat=status) line
        if (status /= 0) exit
       
        do i = 1, len(line)
            if (any(nums == line(i:i))) then
                lineNums = trim(lineNums) // line(i:i)
                exit
            end if
        end do
        do i = len(line), 1, -1
            if (any(nums == line(i:i))) then
                lineNums = trim(lineNums) // line(i:i)
                exit
            end if
        end do
 
        read (lineNums, *) linesum
        result = result + linesum
    end do
    print *, result
    close(iunit)
end program aoc01a