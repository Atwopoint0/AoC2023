program aoc06b
    implicit none
 
    integer :: i, j
    real, dimension(4) :: t, d, record, root1, root2
 
    t = [51, 92, 68, 90]
    d = [222, 2031, 1126, 1225]
    record = 0
 
    do i = 1, 4
        !quadratic formula j*j - t(i)*j + d(i) > 0
        root1(i) = (t(i) - sqrt(t(i)**2 - 4*d(i)))/2
        root2(i) = min((t(i) + sqrt(t(i)**2 - 4*d(i)))/2,t(i))
        record = ceiling(root2) - floor(root1) - 1
    end do
    print*, product(record)
    
end program aoc06b