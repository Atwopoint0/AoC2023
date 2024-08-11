program aoc21
    implicit none
    integer, parameter :: n = 64, m = 131, step = 26501365
    integer, parameter :: dirs(2,4) = reshape([-1,0,1,0,0,-1,0,1], shape=[2,4]) !up=1, down=2, left=3, right=4
    character(len=1) :: garden(m,m)
    integer :: iunit, ios, cur, nxt, prv, now, i, j, k, x
    integer :: start(2), pos_1(2), pos_2(2)
    integer :: next(2, m*m), current(2, m*m), previous(2, m*m), path(-1:m*3)
    integer(8) :: a_0(3), a_2, a_1
 
    open(iunit, file="input.txt", iostat=ios) !parse into array
    do i=1,m
        read(iunit,"(131A1)", iostat=ios) garden(i,:)
    end do
    close(iunit)
    start = findloc(garden,"S") !find where the start is
 
    !part 1
    cur = 1
    prv = 1
    nxt = 0
    current(:,cur) = start
    path(-1:0) = [0,1]
    do i=1,n
        ! little hack to allow revisiting nodes
        now = cur
        do
            loop_1: do j=1,4
                pos_1 = current(:,cur)+dirs(:, j)
                if (check(pos_1)) then
                    if (garden(pos_1(1), pos_1(2)) /= "#") then
                        do k=1,prv
                            if(all(previous(:, k) == pos_1)) cycle loop_1
                        end do
                        do k=1,nxt
                            if(all(next(:, k) == pos_1)) cycle loop_1
                        end do
                        nxt = nxt+1
                        next(:,nxt) = pos_1
                    end if
                end if
            end do loop_1
            cur = cur-1
            if (cur == 0) exit
        end do
        prv = now
        cur = nxt
        previous(:, 1:prv) = current(:, 1:prv)
        current(:, 1:cur) = next(:, 1:cur)
        path(i) = path(i-2) + cur
        nxt = 0
    end do
    print*, path(n)
 
    !part 2
    cur = 1
    prv = 1
    nxt = 0
    current(:,cur) = start
    path(-1:0) = [0,1]
 
    ! find up to iteration 3 in the arithmetic sequence
    do i=1,m*3
        now = cur
        do
            loop_2: do j=1,4
                pos_1 = current(:, cur) + dirs(:, j)
                pos_2 = modulo(pos_1-1, m) + 1
                if (garden(pos_2(1), pos_2(2)) /= "#") then
                    do k=1,prv
                        if (all(previous(:, k) == pos_1)) cycle loop_2
                    end do
                    do k=1,nxt
                        if (all(next(:, k) == pos_1)) cycle loop_2
                    end do
                    nxt = nxt+1
                    next(:, nxt) = pos_1
                end if
            end do loop_2
            cur = cur-1
            if (cur == 0) exit
        end do
        prv = now
        cur = nxt
        previous(:, 1:prv) = current(:, 1:prv)
        current(:, 1:cur) = next(:, 1:cur)
        path(i) = path(i-2) + cur
        nxt = 0
    end do
 
    !interpolate quadratic
    a_0 = path(mod(step, m)::m)
    a_1 = a_0(2) - a_0(1)
    a_2 = (a_0(3) - a_0(2)) - (a_0(2) - a_0(1))
    x = step/m
        print*, a_2*x*(x-1)/2 + a_1*x + a_0(1)
 
    contains
        logical function check(pos)result(res)
            integer,intent(in) :: pos(2)
            res = all(pos > 0) .and. all(pos < m+1)
        end function check
end program aoc21
