program aoc07
   implicit none
   integer,parameter::n=1000
   character(len=5)::card(n),s
   integer::score(n),i,j,temp,iunit
   integer(8)::num
   open(unit=iunit,file="input.txt")
   do i=1,n
      read(iunit,*)card(i),score(i)
   end do
   
   !part a
   do i=2,n
      do j=1,n-i+1
         if(compare(card(j),card(j+1),map1,check1)) then
            s=card(j)
            card(j)=card(j+1)
            card(j+1)=s
            temp=score(j)
            score(j)=score(j+1)
            score(j+1)=temp
         end if
      end do
   end do
   num=0
   do i=1,n
      num=num+int(i,8)*score(i)
   end do
   print*, num
   
   !part b
   do i=2,n
      do j=1,n-i+1
         if(compare(card(j),card(j+1),map2,check2))then
            s=card(j)
            card(j)=card(j+1)
            card(j+1)=s
            temp=score(j)
            score(j)=score(j+1)
            score(j+1)=temp
         end if
      end do
   end do
   num=0
   do i=1,n
      num=num+int(i,8)*score(i)
   end do
   print*, num
 
   close(iunit)
   
contains
 
   logical function compare(a,b,map_j,check_j) result(res)
      character(len=1),intent(in)::a(5),b(5)
      integer::card_a,card_b,i
      procedure(map1)::map_j
      procedure(check1)::check_j
      card_a = check_j(a)
      card_b = check_j(b)
      if (card_a>card_b) then
         res=.true.
      else if (card_a<card_b) then
         res=.false.
      else
         do i=1,5
            card_a=map_j(a(i))
            card_b=map_j(b(i))
            if (card_a>card_b) then
               res=.true.
               return
            else if (card_a<card_b) then
               res=.false.
               return
            end if
         end do
         error stop "equality"
      end if
   end function compare
 
   integer function map1(a) result(res)
      character(len=1),intent(in)::a
      select case(a)
        case("A");res=13
        case("K");res=12
        case("Q");res=11
        case("J");res=10
        case("T");res=9
        case("9");res=8
        case("8");res=7
        case("7");res=6
        case("6");res=5
        case("5");res=4
        case("4");res=3
        case("3");res=2
        case("2");res=1
      end select
   end function map1
 
   integer function check1(a) result(res)
      character(len=1),intent(in)::a(5)
      integer::key(13),i,j
      key=0
      do i=1,5
         j=map1(a(i))
         key(j)=key(j)+1
      end do
      res=hand(key)
   end function check1
 
   integer function map2(a) result(res)
      character(len=1),intent(in)::a
      select case(a)
        case("A");res=13
        case("K");res=12
        case("Q");res=11
        case("T");res=10
        case("9");res=9
        case("8");res=8
        case("7");res=7
        case("6");res=6
        case("5");res=5
        case("4");res=4
        case("3");res=3
        case("2");res=2
        case("J");res=1
      end select
   end function map2
 
   integer function check2(a)result(res)
      character(len=1),intent(in)::a(5)
      integer::key(13),i,j,idx
      key=0
      do i=1,5
         j=map2(a(i))
         key(j)=key(j)+1
      end do
      res=hand(key)
      if (any(a=="J")) then
         select case(res)
         case(6);res=7                      ! if 1 joker + quad     OR 4 joker + single
         case(5);res=7                      ! if 2 joker + triple   OR 3 joker + pair
         case(4);res=6                      ! if 1 joker + triple   OR 3 joker + single
         case(3);res=merge(6,5,key(1)==2)   ! if 2 joker + pair     OR 1 joker + 2 pair
         case(2);res=4                      ! if 2 joker + single   OR 1 joker + pair
         case(1);res=2                      ! if 1 joker + single
         end select
      end if
   end function check2
 
   integer function hand(key)result(res)
      integer,intent(in)::key(13)
      if (any(key==5)) then                  ; res=7 ; return ; end if  !five of kind
      if (any(key==4)) then                  ; res=6 ; return ; end if  !four of kind
      if (any(key==3).and.any(key==2)) then  ; res=5 ; return ; end if  !full house
      if (any(key==3)) then                  ; res=4 ; return ; end if  !three of kind
      if (count(key==2)==2) then             ; res=3 ; return ; end if  !two pair
      if (any(key==2)) then                  ; res=2 ; return ; end if  !one pair
      res=1
   end function hand
 
end program aoc07