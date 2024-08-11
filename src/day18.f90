program aoc18
    use iso_fortran_env, only : ip => int64
    implicit none
    integer :: iunit, ios, idx, isize, i, dist_1, dist_2, dir_1, dir_2
    integer(ip) :: circ_1, circ_2, area_1, area_2, part_1, part_2
    character(20) :: line
    character(1) :: dir
    integer(ip), allocatable :: map_1(:,:), map_2(:,:)
    integer, parameter :: DIRS(2,4) = reshape([0,1,1,0,0,-1,-1,0],[2,4])
    
    open(unit=iunit, file='input.txt', iostat=ios)
    circ_1 = 0
    circ_2 = 0
    isize = 0
    
    ! find the size of the array
    do
        read(iunit, *, iostat=ios)
        if (is_iostat_end(ios)) exit
        isize = isize+1
    end do
    rewind(iunit)
 
    allocate(map_1(2, isize+1))
    allocate(map_2(2, isize+1))
    map_1(:,1) = [1, 1]
    map_2(:,1) = [1, 1]
    
    ! parsing
    do i=1, isize
        read(iunit, '(A)', iostat=ios) line
        idx = index(line,'(')
        dir = line(1:1)
        select case (dir)
            case ('R') 
                dir_1 = 1
            case ('D') 
                dir_1 = 2
            case ('L') 
                dir_1 = 3
            case ('U') 
                dir_1 = 4
            end select
        read(line(3:idx-2),'(I10)') dist_1
        map_1(:,i+1) = map_1(:,i) + DIRS(:,dir_1) * dist_1
        circ_1 = circ_1 + dist_1
        
        read(line(idx+7:idx+7),'(I1)') dir_2
        dir_2 = dir_2 + 1
        dist_2 = hexstr2int(line(idx+2:idx+6))
        map_2(:,i+1) = map_2(:,i) + DIRS(:,dir_2) * dist_2
        circ_2 = circ_2 + dist_2
    end do
    
    ! error handling
    if (mod(circ_1,2) /= 0) print*, 'error: circumference not even in part 1'
    if (mod(circ_2,2) /= 0) print*, 'error: circumference not even in part 2'
    if (any(map_1(:,1) /= map_1(:,size(map_1,2)))) print*, 'error: start does not equal end in part 1'
    if (any(map_2(:,1) /= map_1(:,size(map_2,2)))) print*, 'error: start does not equal end in part 2'
    
    ! apply Pick's theorem
    area_1 = shoelace(map_1)
    part_1 = area_1 + circ_1/2 + 1
    print*, part_1
    
    area_2 = shoelace(map_2)
    part_2 = area_2 + circ_2/2 + 1
    print*, part_2
 
contains
    integer function hexstr2int(str) result(res) !convert hexadecimal string to integer
        character(len=*), intent(in) :: str
        integer, parameter :: N = 5, BASE = 16
        character(len=*), parameter :: DIGITS = "0123456789abcdef"
        integer :: i, j
 
        if (len(str)/=N) error stop 'hexstr2int - unexpected length'
        res = 0
        do i = N, 1, -1
            j = scan(DIGITS, str(i:i))
            if (j==0) error stop 'hexstr2int - unexpected character'
            res = res + BASE**(N-i)*(j-1)
        end do
    end function hexstr2int
 
    function shoelace(map) result(area) !shoelace formula
        integer(ip), intent(in) :: map(:,:)
        integer(ip) :: area
        integer :: i
 
        area = 0
        do i=1,size(map,2)-1
            area = area + (map(2,i)+map(2,i+1))*(map(1,i)-map(1,i+1))
        end do
        if (mod(area,2)/=0) error stop 'shoelace - cannot divide area by two'
        area = abs(area)/2
    end function shoelace
end program aoc18