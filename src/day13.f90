program aoc13
    implicit none
    character(len=999)::string
    integer :: i, iunit, ios, j, n, m, diff, part_1, part_2
    character(len=:), allocatable:: thread
    character(len=1), allocatable:: mirror(:,:)
    open(iunit,file="input.txt")
    part_1=0
    part_2=0
    
    do
        string = "" !parsing
        read(iunit,"(A)",iostat=ios) string
        if (is_iostat_end(ios)) exit
        n = len_trim(string)
        if (n==0) exit
        backspace(iunit)
        
        thread = "" !put everything into a line
        do
            read(iunit,"(A)",iostat=ios) string
            if (len_trim(string)==0 .or. is_iostat_end(ios)) exit
            thread = thread // string(1:n)
        end do
        m = len(thread)/n !convert to appropriate array
        allocate(mirror(n,m))
        mirror(:,:) = reshape(string_to_array(thread), shape=[n,m])
        
        do i=1,m/2+mod(m/2) !check rows
            diff = count(mirror(:,1:i) /= mirror(:,2*i:i+1:-1))
            if (diff==0) then
                part_1 = part_1 + i*100
            else if(diff==1) then
                part_2 = part_2 + (i)*100
            end if
        end do
        do i=m,m/2-mod(m/2), -1
            diff = count(mirror(:,i:m) /= mirror(:,i-1:(2*i-m-1):-1))
            if (diff==0)then
                part_1 = part_1 + (i-1)*100
            else if(diff==1) then
                part_2 = part_2 + (i-1)*100
            end if
        end do
        
        do i=1,n/2+mod(n/2) !check columns
            diff = count((mirror(1:i,:) /= mirror(2*i:i+1:-1,:)))
            if (diff==0) then
                part_1 = part_1 + i
            else if (diff==1) then
                part_2 = part_2 + i
            end if
        end do
        do i=n,n/2-mod(n/2), -1
            diff = count((mirror(i:n,:) /= mirror(i-1:(2*i-n-1):-1,:)))
            if (diff==0) then
                part_1 = part_1 + (i-1)
            else if (diff==1) then
                part_2 = part_2 + (i-1)
            end if
        end do
        
        deallocate(mirror)
    end do
    print*, part_1
    print*, part_2
   
contains
    function string_to_array(data) result(map)
        character(len=*), intent(in) :: data
        character(1):: map(len_trim(data))
        integer :: i
        do i=1, len_trim(data)
            map(i) = data(i:i)
        end do
    end function string_to_array
end program aoc13