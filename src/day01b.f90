program aoc01b
    implicit none
    
    integer :: status, i, linenumpos, linewordpos, linesum, result, iunit, ios
    integer, parameter :: linemax = 100
    character(len=linemax) :: line
    character(len=999) :: linenums, onenums
    character(len=1), dimension(9) :: nums = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    character(len=5), dimension(9) :: words = ["  one", "  two", "three", " four", " five", "  six", "seven", "eight", " nine"]
    integer, dimension(9) :: pos

    status = 0
    result = 0
    
     open(newunit=iunit, file='input.txt', status='OLD', iostat = ios)
     if (ios /= 0) print*, 'error'

    do while (status == 0)
        linesum = 0
        linenums = ''

        read (iunit, '(a)', iostat=status) line
        if (status /= 0) exit
        
        ! go through each line and find the minimal interesting stuff
        pos = linemax 
        do i = 1,size(nums)
            linenumpos = index(trim(line), nums(i));
            if (linenumpos == 0) linenumpos = linemax

            linewordpos = index(trim(line), trim(adjustl(words(i))));
            if (linewordpos == 0) linewordpos = linemax

            pos(i) = min(linenumpos, linewordpos)
        end do
        write (linenums, '(i0)') minloc(pos)
        
        ! go through each line and find the maximal interesting stuff
        pos = 0
        do i = 1,size(nums)
            linenumpos = index(trim(line), nums(i), back=.true.);

            linewordpos = index(trim(line), trim(adjustl(words(i))), back=.true.);

            pos(i) = max(linenumpos, linewordpos)
        end do
        write (onenums, '(i0)') maxloc(pos)
        linenums = trim(linenums) // trim(onenums)

        read (linenums, *) linesum
        result = result + linesum
    end do
    print *, result
    close(iunit)
end program aoc01b