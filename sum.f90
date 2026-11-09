program sum_to_n
  implicit none
  integer(kind=8) :: n, i, total

  write(*, '(A)', advance='no') 'Enter N: '
  read(*, *) n

  if (n < 1) then
    write(*, '(A)') 'N must be a positive integer.'
    stop
  end if

  total = 0
  do i = 1, n
    total = total + i
  end do

  write(*, '(A, I0, A, I0)') 'Sum of 1 to ', n, ' = ', total
  write(*, '(A, I0)') 'Check (N*(N+1)/2) = ', n * (n + 1) / 2
end program sum_to_n
