program aoc10
 
    implicit none
  
    ! east = 1, south = 2, west = 3, north = 4, Chinese style =)
    integer, parameter :: moveDirections(2,4) = reshape([0,1, 1,0, 0,-1, -1,0],[2,4])
    ! pipe directions
    character(len=*), parameter :: CHARS='|-LJ7F'
    ! | = NS, - = EW, L = NE, J = NW, 7 = SW, F = SE
    integer, parameter :: pipeDirections(2,6) = reshape([2,4, 1,3, 1,4, 3,4, 2,3, 1,2],[2,6])
    call day10('input.txt')
 
contains
 
    subroutine day10(file)
    
        character(len=*), intent(in) :: file
        character(len=1), allocatable :: map(:,:)
        integer :: i, isInvalid, part1, part2
        integer, dimension(2) :: start, x, prev, next
        integer, allocatable :: path(:)
        map =  make_map(file)
 
        ! find S and read adjacent pipes to find initial directions
        start = findloc(map, 'S')
        do i=1,4
            prev = start + moveDirections(:,i)
            x = next_pos(prev, start, map, isInvalid)
            if (isInvalid==0) exit
        end do
        path = [start(1), start(2), prev(1), prev(2), x(1), x(2)]
 
        ! follow the path positions until S reached
        part1 = 2
        do
            next = next_pos(x, prev, map, isInvalid)
            if (isInvalid==2) error stop 'no loop found'
            if (isInvalid==1) exit
            part1 = part1 + 1
            path = [path, next(1), next(2)]
            prev = x
            x = next
        end do
        
        ! once loop is found, divide length by two to find furthest distance
        if (mod(part1,2)/=0) error stop 'loop has odd length'
        part1 = part1/2
        print*, part1
 
        
        ! use shoelace formula and subtract the number of the pipe squares
        ! absolute value to take care of winding number (should not happen because no intersections?)
        part2 = abs(shoelace(path))-part1+1
        print*, part2
        
        end subroutine day10
 
    function shoelace(arr) result(area)
        integer, intent(in) :: arr(:)
        integer :: area
        integer :: i
        integer :: xy(2,size(arr)/2)
 
        area = 0
        if (mod(size(arr),2)/=0) error stop 'cannot use shoelace on odd array'
        xy = reshape(arr,[2,size(arr)/2])
        do i=1,size(xy,2)-1
            area = area + (xy(2,i)+xy(2,i+1))*(xy(1,i)-xy(1,i+1))
        end do
        if (mod(area,2)/=0) error stop 'cannot divide by two for some reason'
        area = area/2
    end function shoelace
 
    function next_pos(x, prev, map, isInvalid) result(next)
        integer, intent(in) :: x(2), prev(2)
        character(len=1), intent(in) :: map(:,:)
        integer, intent(out), optional :: isInvalid
        integer :: next(2)
 
        integer :: x1(2), x2(2), p, isInvalid_0
 
        isInvalid_0 = 0
        p = scan(CHARS,map(x(1),x(2)))
        if (p==0) then
            isInvalid_0 = 1
        else
            x1 = x + moveDirections(:,pipeDirections(1,p))
            x2 = x + moveDirections(:,pipeDirections(2,p))
            if (all(x1==prev)) then
                next = x2
            else if (all(x2==prev)) then
                next = x1
            else
                isInvalid_0 = 2
            end if
        end if
 
        select case(isInvalid_0)
            case(0) ! nice
                continue
            case(1) ! Found 'S' or input is wrong
                if (.not. present(isInvalid)) error stop 'next_pos: pipe is not valid'
            case(2) ! loop is broken
                if (.not. present(isInvalid)) error stop 'next_pos: pipe is not connected'
            case default ! something has gone really wrong
                error stop 'impossible'
            end select
            
        if (present(isInvalid)) isInvalid = isInvalid_0
    end function next_pos
  
 
    function  make_map(file) result(mapArray)
        character(len=*), intent(in) :: file
        character(len=1), allocatable :: mapArray(:,:)
        integer :: file_id, nrow, ncol, ios, i
        character(len=9999) :: line
 
        ! read number of rows/columns and ensure same length
        open(newunit=file_id, file=file, status='old')
        nrow = 0
        ncol = -1
        do
            read(file_id,'(a)',iostat=ios) line
            if (ios /= 0) exit
            if (ncol==-1) ncol = len_trim(line)
            if (len_trim(line) /= ncol) error stop ' make_map - input is misshapen'
            nrow = nrow + 1
        end do
        
        ! fill the array data
        allocate(mapArray(ncol,nrow))
        rewind(file_id)
        do i=1,nrow
            read(file_id,'(*(a))') mapArray(:,i)
        end do
        close(file_id)
        mapArray = transpose(mapArray)
    end function  make_map
    
end program aoc10