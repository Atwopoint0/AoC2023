program aocday15a
    implicit none
    character(len=10) :: line
    character :: char
    integer :: iunit, ios, part_1
 
    line = ""
    part_1 = 0
    open(unit = iunit, file = 'input.txt', iostat=ios)
    do
        read (iunit, '(a)', advance="no", iostat=ios) char
        if (char == "," .or. is_iostat_end(ios)) then
            part_1 = part_1 + hash_alg(line)
            line = ""
        else
            line = trim(line) // char
        end if
        if (is_iostat_end(ios)) exit
    end do
    print*, part_1
    
contains
    integer function hash_alg(label)
        character(len=*), intent(in) :: label
        integer :: i
        hash_alg = 0
        do i = 1, len_trim(label)
            hash_alg = mod((hash_alg + ichar(label(i:i))) * 17, 256)
        end do
    end function
end program aocday15a