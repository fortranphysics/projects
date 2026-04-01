program randomocc
    integer :: unit_num, hit
    real, parameter :: side_len=10000
    real :: r_1, r_2
    integer, parameter :: steps= 4*side_len, runs=1000
    real, dimension(2,steps) :: path_1, path_2
    real, dimension(steps) :: rad
    real, dimension(runs) :: rad_list
    do j=1, runs
        path_1(1,1)=0
        path_1(2,1)=1
        path_2(1,1)=side_len
        path_2(2,1)=side_len
        hit= 0
        rad(1)=side_len
        do i=2, steps
            call random_number(r_1)
            call random_number(r_2)
            if (r_1 > 0 .and. r_1 < 1.0/3) then
                path_1(1,i) = path_1(1,i-1)+1
                path_1(2,i) = path_1(2,i-1)
            else if (r_1 >= 1.0/3 .and. r_1 < 2.0/3 ) then
                path_1(2,i) = path_1(2, i-1)+1 
                path_1(1,i) = path_1(1,i-1)
            else if (r_1 >= 2.0/3) then
                path_1(1,i) = path_1(1,i-1)+1
                path_1(2,i) = path_1(2,i-1)+1
            else
                path_1(1,i) = path_1(1,i-1)
                path_1(2,i) = path_1(2,i-1)
            end if
            if (r_2 > 0 .and. r_2 < 1.0/3) then
                path_2(1,i) = path_2(1,i-1)-1
                path_2(2,i) = path_2(2,i-1)
            else if (r_2 >= 1.0/3 .and. r_2 < 2.0/3 ) then
                path_2(2,i) = path_2(2, i-1)-1 
                path_2(1,i) = path_2(1,i-1)
            else if (r_2 >= 2.0/3) then
                path_2(2,i) = path_2(2, i-1)-1
                path_2(1,i) = path_2(1,i-1)-1
            else
                path_2(2,i) = path_2(2, i-1)
                path_2(1,i) = path_2(1,i-1)
            end if
            rad(i) = sqrt((path_1(1,i) - path_2(1,i))**2 + (path_1(2,i) - path_2(2,i))**2) 
        end do
        rad_list(j)=minval(rad)
    end do
    print *, sum(rad_list)/size(rad_list), maxval(rad_list), minval(rad_list)
end program



