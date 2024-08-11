program aoc02a
    implicit none
    
    integer :: i, lineNum, colourNum, total, iunit, ios
    integer, dimension(3) :: colourBalls
    character(len=999) :: line
    character(len=10) :: buffer
 
    ios = 0
    total = 0
    lineNum = 1
 
    open(unit=iunit, file='input.txt', iostat = ios)
    do while (ios == 0)
        read (iunit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        
        ! move to next game
        buffer = ''
        colourNum = 0
        colourBalls = [12, 13, 14]
        
        ! remove initial junk
        i = index(line, ": ") + 1
        do while (i <= len(trim(line)))
            i = i + 1
            
            ! sample check
            if (line(i:i) == "," .or. line(i:i) == ";" .or. i > len(trim(line))) then
                if (trim(buffer) == 'red') then
                    colourBalls(1) = colourBalls(1) - colourNum
                    if (colourBalls(1) < 0) exit;
                else if (trim(buffer) == 'green') then
                    colourBalls(2) = colourBalls(2) - colourNum
                    if (colourBalls(2) < 0) exit;
                else if (trim(buffer) == 'blue') then
                    colourBalls(3) = colourBalls(3) - colourNum
                    if (colourBalls(3) < 0) exit;
                else
                    print *, "bad data"
                end if
                
                ! move to next sample
                buffer = '';
                colourNum = 0;
                if (i > len(line)) exit;
                if (line(i:i) == ";") then
                    colourBalls = [12, 13, 14]
                end if
                
                ! remove the whitespace after delimiters
                i = i + 1 
            else if (line(i:i) == " ") then
                read (buffer, *) colourNum
                buffer = ''
            else
                buffer = trim(buffer) // line(i:i)
            end if
        end do
 
        ! game check
        if (minval(colourBalls) >= 0) then
            total = total + lineNum
        end if
        lineNum = lineNum + 1
        
    end do
 
    print *, total
    close(iunit)
end program aoc02a