module day14
    implicit none
 
    enum, bind(C)
        enumerator :: empty = 0
        enumerator :: round = 1
        enumerator :: cube  = 2
    end enum
    contains
    pure function load(map) result(weight)
        integer, intent(in) :: map(:, :)
        integer :: weight
        integer :: map_copy(size(map, 1), size(map, 2))
        integer :: cost(size(map, 2))
        integer :: i
        map_copy = map
        where (map_copy == cube)
            map_copy = empty
        end where
        cost = [(i, i = size(map, 2), 1, -1)]
        weight = sum(matmul(map_copy, cost))
    end function load
end module day14
 
program aoc14
    use day14
    implicit none
    character(len=999) :: line
    integer, allocatable :: map_part_1(:, :), map_part_2(:, :)
    integer :: load_hist(999)
    integer :: ios, iunit, cols, rows, i, j, k, l, spin_cycle, cycle_length, cycle_offset
 
    open(unit=iunit, file='input.txt', iostat=ios)
    cols = 0
    rows = 0
    do !parsing
        read(iunit, '(A)', iostat=ios) line
        if (cols == 0) cols = len(trim(line))
        if (ios /= 0) exit
        rows = rows + 1
    end do
    allocate(map_part_1(cols, rows))
    rewind(iunit)
    do i = 1, rows !fill the map data
        read(iunit, '(A)', iostat=ios) line
        do j = 1, cols
            if (line(j:j) == '#') then
                map_part_1(j, i) = cube
            else if (line(j:j) == 'O') then
                map_part_1(j, i) = round
            else
                map_part_1(j, i) = empty
            end if
        end do
    end do
    close(iunit)
 
    do i = 2, rows !shift the rounded rocks north
        do k = 2, rows - (i - 2)
            do j = 1, cols
                if (map_part_1(j, k) == round .and. map_part_1(j, k - 1) == empty) then
                    map_part_1(j, k - 1) = map_part_1(j, k)
                    map_part_1(j, k) = empty
                end if
            end do
        end do
    end do
    print*, load(map_part_1)
    
    map_part_2 = map_part_1
    outer_loop: do spin_cycle = 1, size(load_hist)
        do i = 2, rows !shift north
            do k = 2, rows - (i - 2)
                do j = 1, cols
                    if (map_part_2(j, k) == round .and. map_part_2(j, k - 1) == empty) then
                        map_part_2(j, k - 1) = map_part_2(j, k)
                        map_part_2(j, k) = empty
                    end if
                end do
            end do
        end do
        do j = 2, cols !shift west
            do k = 2, cols - (j - 2)
                do i = 1, rows
                    if (map_part_2(k, i) == round .and. map_part_2(k - 1, i) == empty) then
                        map_part_2(k - 1, i) = map_part_2(k, i)
                        map_part_2(k, i) = empty
                    end if
                end do
            end do
        end do
        do i = rows - 1, 1, -1 !shift south
            do k = rows - 1, 1 + (rows - i - 1), -1
                do j = 1, cols
                    if (map_part_2(j, k) == round .and. map_part_2(j, k + 1) == empty) then
                        map_part_2(j, k + 1) = map_part_2(j, k)
                        map_part_2(j, k) = empty
                    end if
                end do
            end do
        end do
        do j = cols - 1, 1, -1 !shift east
            do k = cols - 1,  1 + (cols - j - 1), -1
                do i = 1, rows
                    if (map_part_2(k, i) == round .and. map_part_2(k + 1, i) == empty) then
                        map_part_2(k + 1, i) = map_part_2(k, i)
                        map_part_2(k, i) = empty
                    end if
                end do
            end do
        end do
        
        load_hist(spin_cycle) = load(map_part_2)
        do l = 2, spin_cycle/2 !stop cycling once stable
            if (all(load_hist(spin_cycle - l + 1 : spin_cycle) == load_hist(spin_cycle - 2 * l + 1 : spin_cycle - l) )) then
                cycle_length = l
                cycle_offset = spin_cycle - l + 1
                exit outer_loop
            end if
        end do
    end do outer_loop
 
    print*, load_hist(cycle_offset + mod(1000000000 - cycle_offset, cycle_length))
 
end program aoc14