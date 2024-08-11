program aoc15
    implicit none
    type Lens
        character(len=10), allocatable :: lenses(:)
    end type
 
    character(len=10) :: label
    character :: char, op, focal_num
    integer :: iunit, ios, box_pos, i, j, part_2, focal
    type(Lens) :: boxes(256)
    logical :: present
 
    label = ""
    op = ""
    focal_num = ""
    
    open(unit=iunit, file='input.txt', iostat=ios)
    do
        read (iunit, '(a)', advance="no", iostat=ios) char
        if (char == "," .or. is_iostat_end(ios)) then !end of box
            box_pos = hash_alg(label)
            if (op == "-") then !remove lens and shift remaining lenses
                if (allocated(boxes(box_pos)%lenses)) then
                    do i = 1, size(boxes(box_pos)%lenses)
                        if (index(boxes(box_pos)%lenses(i), trim(label)) /= 0) then
                            boxes(box_pos)%lenses = [boxes(box_pos)%lenses(1:i-1), boxes(box_pos)%lenses(i+1:)]
                            exit
                        end if
                    end do
                end if
            else if (op == "=") then !split into two cases
                if (allocated(boxes(box_pos)%lenses)) then !lens found, replace old lens and replace with new lens
                    present = .false.
                    do i = 1, size(boxes(box_pos)%lenses) !start by removing old lens
                        if (index(boxes(box_pos)%lenses(i), trim(label)) /= 0) then
                            boxes(box_pos)%lenses(i:i) = trim(label) // " " // focal_num
                            present = .true.
                            exit
                        end if
                    end do
                    if (.not. present) then !then add new lens
                        boxes(box_pos)%lenses = [boxes(box_pos)%lenses, trim(label) // " " // focal_num]
                    end if
                else !no old lens found, add new lens at the end
                    boxes(box_pos)%lenses = [trim(label) // " " // focal_num]
                end if
            else 
                error stop 'broken'    
            end if
 
            label = ""
            op = ""
            focal_num = ""
        else !middle of box
            if (char == "-" .or. char == "=") then !reached op
                op = char
            else if (op == "") then
                label = trim(label) // char !afix the lens id
            else
                focal_num = char !define focal number of lens
            end if
        end if
        if (is_iostat_end(ios)) exit
    end do
 
    part_2 = 0
    do i = 1, size(boxes)
        if (allocated(boxes(i)%lenses)) then
            do j = 1, size(boxes(i)%lenses)
                read (boxes(i)%lenses(j), *) label, focal
                part_2 = part_2 + (i * j * focal)
            end do
        end if
    end do
    print *, part_2
    
contains
    integer function hash_alg(label)
        character(len=*), intent(in) :: label
        integer :: i
        hash_alg = 0
        do i = 1, len_trim(label)
            hash_alg = mod((hash_alg + ichar(label(i:i))) * 17, 256)
        end do
        hash_alg = hash_alg + 1  !add 1 lmao
    end function
end program aoc15