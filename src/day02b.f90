program aocday2b
    implicit none
    
    integer :: ios, i, lineNum, colourNum, total, power, iunit
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
 
        ! recycle the code from part a
        buffer = ''
        colourNum = 0
        colourBalls = 0
 
        i = index(line, ": ") + 1
        do while (i <= len(trim(line)))
            i = i + 1
 
            if (line(i:i) == "," .or. line(i:i) == ";" .or. i > len(trim(line))) then
                if (trim(buffer) == 'red') then
                    if (colourBalls(1) < colourNum) colourBalls(1) = colourNum
                else if (trim(buffer) == 'green') then
                    if (colourBalls(2) < colourNum) colourBalls(2) = colourNum
                else if (trim(buffer) == 'blue') then
                    if (colourBalls(3) < colourNum) colourBalls(3) = colourNum
                else
                    print *, "bad data"
                end if
 
                buffer = '';
                colourNum = 0;
 
                if (i > len(trim(line))) exit;
                i = i + 1 
            else if (line(i:i) == " ") then
                read (buffer, *) colourNum
                buffer = ''
            else
                buffer = trim(buffer) // line(i:i)
            end if
        end do
 
        power = colourBalls(1) * colourBalls(2) * colourBalls(3)
        total = total + power
 
        lineNum = lineNum + 1
    end do
 
    print *, total
end program aocday2b