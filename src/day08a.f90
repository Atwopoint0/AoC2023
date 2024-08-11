program aoc08a
    implicit none
 
    type Map
        character(len=5) :: key, l, r
    endtype
    
    character(len=5), dimension(:), allocatable :: startPosition
    type(Map), dimension(:), allocatable :: maps
    character(len=999) :: line, directions
    character(len=5) :: currPosition
    character :: currDirection
    integer :: status, i, k, m, ios, iunit
    integer(kind=8) :: total
 
    open(unit = iunit, file = 'input.txt', iostat = ios)
    read (iunit, *) line
    directions = trim(line)
    
    do
        read (iunit, '(a)', iostat=ios) line
        if (is_iostat_end(ios)) exit
        if (len_trim(line) == 0) cycle
 
        if (allocated(maps)) then
            maps = [maps, Map(line(1:3), line(8:10), line(13:15))]
        else
            maps = [Map(line(1:3), line(8:10), line(13:15))]
        end if
    end do
 
    startPosition = pack(maps%key, maps%key(3:3) == 'A')
    total = 0
    do k = 1, size(startPosition)
        currPosition = startPosition(k)
        i = 1
        do while (currPosition(3:3) /= 'Z')
            m = mod(i - 1, len_trim(directions)) + 1
            currDirection = directions(m:m)
            if (currDirection == 'L') then
                currPosition = maps(findloc(maps%key, currPosition, dim=1))%l
            else if (currDirection == 'R') then
                currPosition = maps(findloc(maps%key, currPosition, dim=1))%r
            else 
                print*, 'cry'
            end if
            i = i + 1
        end do
        
        if (total == 0) then
            total = int8(i - 1)
        else
            total = lcm(total, int8(i - 1))
        end if
    end do
    print *, total
 
contains !Euclidean Algorithm for GCD and LCM
    recursive function gcd (a, b) result (out)
        integer(kind=8), intent(in) :: a, b
        integer(kind=8) :: out
        if (a > 0) then
            out = gcd(mod(b, a), a)
        else
            out = b
        end if
    end function
 
    recursive function lcm (a, b) result (out)
        integer(kind=8), intent(in) :: a, b
        integer(kind=8) :: out
        out = (a * b) / gcd(a, b)
    end function
end program aoc08a