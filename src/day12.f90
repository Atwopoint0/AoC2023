program aoc12
    use iso_fortran_env, ip => int64
    implicit none
    character (:), allocatable :: token, istr
    character(100) :: line
    integer, dimension(:), allocatable :: blocks
    integer(ip) :: begin, end, rate, part_1, part_2
    integer :: iunit, ios, idx, inum, i, j, istart, istep, isize
    
    call system_clock(begin, rate)
    open(unit=iunit, file='input.txt', iostat=ios)
    part_1 = 0
    part_2 = 0
    istep = 0
    do
        read(iunit, '(A)', iostat=ios) line !parse everything
        if (is_iostat_end(ios)) exit
        istep = istep + 1
        if (allocated(blocks)) deallocate(blocks)
        idx = index(line,' ')
        token = trim(line(:idx-1))
        istr = trim(line(idx+1:))
        istart = 1
        allocate(blocks(0))
        do i=1, len(istr)
            if (istr(i:i) == ',') then
                read(istr(istart:i-1), '(I10)') inum
                blocks = [blocks, inum]
                istart = i + 1
            end if
        end do
        read(istr(istart:len(istr)), '(I10)') inum
        blocks = [blocks, inum]
        isize = size(blocks)
        part_1 = nonogram(token, blocks) + part_1
        do i=1,4 !duplicate 5 times for part 2
            token = token // '?' // trim(line(:idx-1))
            do j=1,isize
                blocks = [blocks,blocks(j)]
            end do
        end do
        part_2 = nonogram(token, blocks) + part_2
    end do
    close(iunit) !parsing all done
    print*, 'The sum of all', istep, 'nongrams in part 1 is:', part_1
    print*, 'The sum of all', istep, 'nongrams in part 2 is:', part_2
    call system_clock(end)
    print *, "Program took: ", end - begin, " cycles. Where there are ", rate, "cycles/s"
 
contains
    function nonogram(token, blocks) result(paths)
        use iso_fortran_env, ip => int64
        character(:), allocatable, intent(in) :: token
        integer, dimension(:), allocatable, intent(in) :: blocks
        
        type :: ragged_vectors
            integer, allocatable :: vectors(:)
        end type ragged_vectors
        type :: ragged_matrices
            integer, allocatable :: matrices(:,:)
        end type ragged_matrices
        
        integer, dimension(:), allocatable :: least, most, first_broken
        integer, dimension(:,:), allocatable :: left, right
        integer(ip), dimension(:,:), allocatable :: prod
        type(ragged_vectors), allocatable :: valid_locs(:), prune_locs(:)
        type(ragged_matrices), allocatable :: valences(:)
        integer :: i, j, k, n, m, max
        integer(ip) :: paths
    
        max = size(blocks)
        allocate(least(max))
        allocate(most(max))
        least(1) = 1
        most(max) = len(token) - blocks(max) + 1 
        do i=2, max !find the lowest viable locations for each block based upon earlier blocks
            least(i) = least(i-1) + blocks(i-1) + 1
        end do
        do i=max-1,1,-1 !find the highest viable locations for each block based upon later blocks
            most(i) = most(i+1) - blocks(i) - 1
        end do
        
        allocate(valid_locs(max))
        allocate(prune_locs(max))
        do i=1, max !prune all the locations that where the block overlaps with an unbroken spring '.'
            allocate(valid_locs(i)%vectors(most(i) - least(i) + 1))
            do j = least(i),most(i)
                if (index(token(j:j+blocks(i)-1),".") /= 0) then
                    valid_locs(i)%vectors(j+1-least(i)) = 0
                else
                    valid_locs(i)%vectors(j+1-least(i)) = j
                end if
            end do
            allocate(prune_locs(i)%vectors(count(valid_locs(i)%vectors /= 0)))
            prune_locs(i)%vectors = pack(valid_locs(i)%vectors, valid_locs(i)%vectors /= 0)
        end do
        !this is the best part to optimise - pruning makes matrices smaller
    
        allocate(left(size(prune_locs(1)%vectors), 1))
        allocate(right(1, size(prune_locs(max)%vectors)))
        do k=1, size(prune_locs(1)%vectors) !deal with the left matrix - no leading broken springs '#' allowed 
            if (index(token(:prune_locs(1)%vectors(k) - 1),'#') /= 0) then
                left(k,1) = 0
            else 
                left(k,1) = 1
            end if
        end do
        do k=1, size(prune_locs(max)%vectors) !deal with the right matrix - no trailing broken springs '#' allowed
            if (index(token(prune_locs(max)%vectors(k) + blocks(max):),'#') /= 0) then
                right(1,k) = 0
            else 
                right(1,k) = 1
            end if
        end do
        
        allocate(valences(max-1))
        do i=1, max-1 !construct the valency matrices, m by n which depend on the number of pruned locations for the blocks
            m = size(prune_locs(i+1)%vectors)
            n = size(prune_locs(i)%vectors)
            allocate(valences(i)%matrices(m,n))
            do k=1,n !find the first broken spring after the kth location
                allocate(first_broken(n))
                if (index(token(prune_locs(i)%vectors(k) + blocks(i):),"#") /= 0) then
                    first_broken(k) = index(token(prune_locs(i)%vectors(k) + blocks(i):),"#") &
                    &+ prune_locs(i)%vectors(k) + blocks(i) - 1
                else !otherwise set to out of bounds
                      first_broken(k) = len(token) + 1
                end if
                !print*, 'first broken spring for block', i, 'at location', prune_locs(i)%vectors(k), 'is at', first_broken(k)
                do j=1,m !the (j,k)-th entry is equal to 1 if and only if the blocks are not too close nor too far
                    !i.e. adjacent blocks cannot overlap nor contain an erroneous broken spring '#'
                    if  (prune_locs(i+1)%vectors(j) - prune_locs(i)%vectors(k) >= blocks(i) + 1 &
                        &.and. prune_locs(i+1)%vectors(j) <= first_broken(k)) then
                        valences(i)%matrices(j,k) = 1
                    else
                        valences(i)%matrices(j,k) = 0
                    end if
                end do
                deallocate(first_broken)
            end do
        end do
        
        prod = left
        do i=1, max-1 !finally, left multiply every matrix together
            prod = matmul(valences(i)%matrices,prod)
        end do
        prod = matmul(right,prod)
        paths = prod(1,1)
    end function nonogram
end program aoc12