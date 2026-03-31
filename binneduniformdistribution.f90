program stats
    real :: r
    integer :: unit_num
    integer, parameter :: n=10**6, bins=10
    real, dimension(n) :: array
    real, dimension(bins) :: distribution
    do i=1, n
        call random_number(r)
        array(i)= r**.23
    end do
    do i=1, bins
        distribution(i)=0
    end do 
    do i=1, n
        do j=1, bins
            if ((j-1)/real(bins)<array(i) .and. array(i)< j/real(bins)) then 
                distribution(j)=distribution(j)+1
            end if
        end do
    end do
    open(action='write', file="C:\Users\RSFaj\Documents\string.txt", newunit=unit_num, status='replace')
    do i=1, bins
        write(unit_num,*) distribution(i)/n
    end do
    close(unit_num)
    call execute_command_line("gnuplot -c plot.plt")
end program