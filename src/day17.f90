module mod
    use, intrinsic :: iso_fortran_env
    implicit none
    integer, dimension(2), parameter :: north=[-1,0], south=[1,0], east=[0,1], west=[0,-1]
    integer, dimension(2,4), parameter :: directions = reshape([north,south,east,west],[2,4])
    type :: node
        integer, dimension(:,:), allocatable :: steps
        integer, dimension(:), allocatable :: dists
    end type node
    
    ! ignore all this PARSING GARBAGE
    interface pushone
        procedure pushone_i, pushone_c
    end interface pushone
    
    interface readmat
        procedure readmat_i, readmat_c
    end interface readmat
    
    interface resize
        procedure resize_i, resize_c
    end interface resize
 
    ! ignore all this PARSING GARBAGE
    contains
    integer function nlines(iu) 
        integer ios, iu
        nlines=0
        do
            read(iu, *, iostat=ios)
            if (ios.eq.iostat_end) exit
            nlines = nlines + 1
        end do
        rewind(iu)
    end function nlines
    
    subroutine resize_i(array, n)
        integer, dimension(:), allocatable :: array, temp
        integer n, s
        allocate(temp(n))
        s = min(n, size(array))
        temp(1:s) = array(1:s)
        call move_alloc(temp, array)
    end subroutine resize_i
 
    subroutine resize_c(array, n)
        character(len=*), dimension(:), allocatable :: array
        character(len=len(array)), dimension(:), allocatable :: temp
        integer n, s
        allocate(temp(n))
        ! temp = 0
        s = min(n, size(array))
        temp(1:s) = array(1:s)
        call move_alloc(temp, array)
    end subroutine resize_c
    
    subroutine pushone_i(array, value)
        integer, dimension(:), allocatable :: array
        integer value
        integer s
        s = size(array)+1
        call resize(array, s)
        array(s) = value
    end subroutine pushone_i
 
    subroutine pushone_c(array, value)
        character(len=*), dimension(:), allocatable :: array
        character(len=*) value
        integer s
        s = size(array)+1
        call resize(array, s)
        array(s) = value
    end subroutine pushone_c
 
    subroutine push_column(matrix, column)
        integer, dimension(:,:), allocatable :: matrix, temp
        integer column(2), mat_size
        mat_size = size(matrix, dim=2)
        allocate(temp(2, mat_size+1))
        temp(:, :mat_size) = matrix
        temp(:, mat_size+1) = column
        call move_alloc(temp, matrix)
    end subroutine push_column
 
    integer function locate_column(matrix, column)
        integer, dimension(:,:) :: matrix
        integer column(size(matrix, dim=1)), i
        locate_column = 0
        do i=1, size(matrix,dim=2)
            if (all(matrix(:,i).eq.column)) then
                locate_column = i
                return
            end if
        end do
    end function locate_column
 
    subroutine readmat_c(iu, mat, nr, nc)
        character(len=256) l
        character, dimension(:,:), allocatable :: mat
        integer iu, ios, nr, nc, i, j
        nr = nlines(iu)
        read(iu, "(a)", iostat=ios) l
        nc = len_trim(l)
        backspace(iu)
        allocate(mat(nr, nc))
        do j=1, nr
            read(iu, "(*(a))") (mat(j,i), i=1,nc)
        end do
    end subroutine readmat_c
 
    subroutine readmat_i(iu, mat, nr, nc)
        character(len=256) l
        integer, dimension(:,:), allocatable :: mat
        integer iu, ios, nr, nc, i, j
        nr = nlines(iu)
        read(iu, "(a)", iostat=ios) l
        nc = len_trim(l)
        backspace(iu)
        allocate(mat(nr, nc))
        do j=1, nr
            read(iu, "(*(i1))") (mat(j,i), i=1,nc)
        end do
    end subroutine readmat_i
    
    !stop igoring
    ! modified dijkstra algorithm
    subroutine dijkstra(map, num_rows, num_cols, heat_map, toggle)
        integer, dimension(:,:), allocatable :: map
        integer, intent(in) :: num_rows, num_cols
        integer i, j, k, streak, dst, heat, c
        integer pos_cur(2), pos_nxt(2), dir_cur(2), dir_nxt(2), stp(2)
        type(node), target, dimension(:,:), allocatable :: heat_map
        type(node), pointer :: h_cur, h_nxt
        logical toggle
        logical, dimension(num_rows, num_cols) :: visited
 
        if (allocated(heat_map)) deallocate(heat_map)
        allocate(heat_map(num_rows, num_cols))
        do i=1, num_cols
            do j=1, num_rows
                allocate(heat_map(j,i)%steps(2,0))
                allocate(heat_map(j,i)%dists(1))
                heat_map(j,i)%dists(1) = huge(dst)
            end do
        end do
        
        visited = .false.
        ! mark starting point as visited
        visited(1,1) = .true.
        heat_map(1,1)%dists(1) = 0
        call push_column(heat_map(1,1)%steps,[0,0])
        c = 0
        do
            if (.not.any(visited)) exit
            ! find visited spot and unmark visited
            pos_cur = findloc(visited,.true.)
            visited(pos_cur(1),pos_cur(2)) = .false.
            ! loop over all possible paths to this point
            h_cur => heat_map(pos_cur(1), pos_cur(2))
            do j=1,size(h_cur%steps, dim=2)
                stp = h_cur%steps(:,j)
                dst = h_cur%dists(j)
                !check every direction
                do i=1,4
                    dir_cur = directions(:,i)
                    ! dot product magic
                    streak = dot_product(dir_cur, stp)
                    if (.not.toggle) then
                        ! part 1 - cannot go in one direction for more than 3
                        if (streak < 0 .or. streak >= 3) cycle
                    else if (toggle) then
                        ! part 2 - cannot go in one direction for more than 10
                        if (streak < 0 .or. streak >= 10) cycle
                        if (c > 0) then
                            ! part 2 - if we have just changed direction, keep going for 4
                            if (streak == 0 .and. sum(abs(stp)) < 4) cycle
                        end if
                    end if
                    pos_nxt = pos_cur + dir_cur
                    ! out of bounds check
                    if ((pos_nxt(1) < 1 .or. pos_nxt(1) > num_rows) .or. (pos_nxt(2) < 1 .or. pos_nxt(2) > num_cols)) cycle
                    
                    dir_nxt = dir_cur * (streak + 1)
                    heat = dst + map(pos_nxt(1),pos_nxt(2))
                    h_nxt => heat_map(pos_nxt(1),pos_nxt(2))
                    k = locate_column(h_nxt%steps,dir_nxt)
                    if (k == 0) then
                        ! add path at node
                        call push_column(h_nxt%steps,dir_nxt)
                        if (h_nxt%dists(1) == huge(heat)) then
                            h_nxt%dists(1) = heat
                        else
                            call pushone(h_nxt%dists, heat)
                        end if
                        ! mark visited
                        visited(pos_nxt(1),pos_nxt(2)) = .true.
                    else
                        ! replace if we have improvement
                        if (heat < h_nxt%dists(k)) then
                            h_nxt%dists(k) = heat
                            ! re-mark as visited
                            visited(pos_nxt(1),pos_nxt(2)) = .true.
                        end if
                    end if
                end do
            end do
            ! :)))
            if (c == 0) c = 1
        end do
    end subroutine dijkstra
 
    subroutine part1()
        implicit none
        integer, dimension(:,:), allocatable :: map
        integer iunit, heat_loss, num_rows, num_cols
        type(node), dimension(:,:), allocatable :: heat_map
 
        heat_loss = 0
        open(iunit, file="input.txt", status='old')
        call readmat(iunit, map, num_rows, num_cols)
        close(iunit)
 
        call dijkstra(map, num_rows, num_cols, heat_map, .false.)
        heat_loss = minval(heat_map(num_rows,num_cols)%dists)
        print*, heat_loss
    end subroutine part1
 
    subroutine part2()
        implicit none
        integer, dimension(:,:), allocatable :: map
        integer iunit, heat_loss, num_rows, num_cols
        type(node), dimension(:,:), allocatable :: heat_map
 
        heat_loss = 0
        open(iunit, file="input.txt", status='old')
        call readmat(iunit, map, num_rows, num_cols)
        close(iunit)
 
        call dijkstra(map, num_rows, num_cols, heat_map, .true.)
        heat_loss = minval(heat_map(num_rows,num_cols)%dists)
        print*, heat_loss
    end subroutine part2
 
end module mod
 
program aoc17
    use mod
    implicit none
    integer :: begin, end, rate
    call system_clock(begin, rate)
    call part1()
    call system_clock(end)
    print *, "part 1 took: ", end - begin, " cycles. where there are ", rate, "cycles/s"
    call system_clock(begin, rate)
    call part2()
    call system_clock(end)
    print *, "part 2 took: ", end - begin, " cycles. where there are ", rate, "cycles/s"
end program aoc17