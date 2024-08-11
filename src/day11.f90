program aoc11
    implicit none
    integer, parameter :: n=140
    integer :: i, j, iunit, ios
    type pair
        integer :: x,y
    end type pair
    type(pair), allocatable :: galaxy(:)
    integer, allocatable :: col(:), row(:)
    integer(8) :: L1_metric, part_1, part_2
    character(len=1) :: map(n,n)
    open(unit = iunit,file = "input.txt", iostat = ios)
    
    ! parsing
    do i=1,n
        read(iunit,"(140A1)")map(:,i)
    end do
    allocate(galaxy(0))
    allocate(col(0))
    allocate(row(0))
    do i=1,n
        if (all(map(:,i)==".")) col=[col,i]
        if (all(map(i,:)==".")) row=[row,i]
    end do
    do i=1,n
        do j=1,n
            if(map(j,i)=="#")then
                galaxy=[galaxy,pair(j,i)]
            end if
        end do
    end do
    
    part_1=0 !part 1
    do i=1,size(galaxy)
        do j=i+1,size(galaxy)
            associate(ix => galaxy(i)%x, iy => galaxy(i)%y, jx => galaxy(j)%x, jy => galaxy(j)%y)
                L1_metric = abs(jx-ix) + abs(jy-iy)& 
                &+ count(min(ix,jx) < row.and.row < max(ix,jx))& 
                &+ count(min(iy,jy) < col.and.col < max(iy,jy))
                part_1 = part_1+L1_metric
            end associate
        end do
    end do
    print*, part_1
    
    part_2 = 0 !part 2
    do i=1,size(galaxy)
        do j=i+1,size(galaxy)
            associate(ix => galaxy(i)%x, iy => galaxy(i)%y, jx => galaxy(j)%x, jy => galaxy(j)%y)
                L1_metric = abs(jx-ix)+abs(jy-iy)& 
                &+ count(min(ix,jx) < row.and.row < max(ix,jx))*(1000000-1)& 
                &+ count(min(iy,jy) < col.and.col < max(iy,jy))*(1000000-1)
                part_2 = part_2 + L1_metric
            end associate
        end do
    end do
    print*, part_2
    
    close(iunit)
end program aoc11