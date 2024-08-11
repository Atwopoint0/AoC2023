program aoc04
    implicit none
    character :: char
    character(len=5) :: buffer
    integer :: ios, total, startGame, cardNum, iunit
    integer, dimension(:), allocatable :: winningNum, wonNum
 
    total = 0
    ios = 0
 
    startGame = 1
    buffer = ''
 
    open(unit=iunit, file='input.txt', iostat = ios)
    
    do
        read (iunit, '(a)', advance='no', iostat=ios) char
        if (is_iostat_end(ios)) char = ' '
        if (char == ':') then
            startGame = 2
        else if (char == ' ' .and. len_trim(buffer) > 0) then
            read (buffer, *) cardNum
            buffer = '';
            if (startGame == 2) then
                if (allocated(winningNum)) then
                    winningNum = [winningNum, cardNum]
                else
                    winningNum = [cardNum]
                end if
            else
                if (any(winningNum == cardNum)) then
                    if (allocated(wonNum)) then
                        wonNum = [wonNum, cardNum]
                    else
                        wonNum = [cardNum]
                    end if
                end if
            end if
        else if (char == '|') then
            startGame = 3
        else if (char /= ' ' .and. startGame /= 1) then
            buffer = trim(buffer) // char
        else
            print *, 'break at [', char, ']'
        end if
 
        if (ios /= 0) then
            if (allocated(wonNum)) total = total + (2**(size(wonNum) - 1))
 
            startGame = 1
            buffer = ''
            if (allocated(winningNum)) deallocate(winningNum)
            if (allocated(wonNum)) deallocate(wonNum)
        end if
 
        if (is_iostat_end(ios)) exit
    end do
 
    print *, total
    close(iunit)
end program aoc04