! Task achievement grading system for HW1 (Tasks 1-4)
! The user enters the actual results of each task; the program checks them
! automatically and awards 20 points per task that meets its criterion (80 total).
program grade_tasks
  implicit none
  integer, parameter :: ntask = 4, points = 20
  integer(kind=8), parameter :: nmax = 3000000000_8   ! keeps N*(N+1) within 64-bit range
  integer :: task_score(ntask), total, t, k
  character(len=40) :: task_name(ntask)
  character(len=200) :: reason(ntask)
  integer(kind=8) :: n, s, n2(3), s2(3), commits
  character(len=256) :: url
  logical :: ok, eof

  task_name(1) = 'Task 1: Write, compile and run program'
  task_name(2) = 'Task 2: Three random verifications'
  task_name(3) = 'Task 3: Git version control'
  task_name(4) = 'Task 4: Push to GitHub'
  task_score = 0
  reason = ''

  write(*, '(A)') '===== HW1 Task Achievement Grading ====='
  write(*, '(A)') 'Enter the actual results of each task.'

  ! ---- Task 1: one run of the sum program ----
  write(*, '(/, A)') '--- ' // trim(task_name(1)) // ' ---'
  call read_int('N used in the run: ', n, eof)
  if (.not. eof) call read_int('Program output (sum): ', s, eof)
  if (eof) then
    reason(1) = 'missing input'
  else
    call check_sum(n, s, ok, reason(1))
    if (ok) task_score(1) = points
  end if

  ! ---- Task 2: three random runs ----
  write(*, '(/, A)') '--- ' // trim(task_name(2)) // ' ---'
  ok = .true.
  do k = 1, 3
    if (.not. eof) call read_int('Test ' // achar(48 + k) // ' N: ', n2(k), eof)
    if (.not. eof) call read_int('Test ' // achar(48 + k) // ' output: ', s2(k), eof)
  end do
  if (eof) then
    reason(2) = 'missing input'
  else
    do k = 1, 3
      call check_sum(n2(k), s2(k), ok, reason(2))
      if (.not. ok) then
        reason(2) = 'test ' // achar(48 + k) // ': ' // trim(reason(2))
        exit
      end if
    end do
    if (ok .and. (n2(1) == n2(2) .or. n2(1) == n2(3) .or. n2(2) == n2(3))) then
      ok = .false.
      reason(2) = 'the three N values must be different'
    end if
    if (ok) then
      task_score(2) = points
      reason(2) = 'all 3 tests correct'
    end if
  end if

  ! ---- Task 3: number of git commits (original + commented version) ----
  write(*, '(/, A)') '--- ' // trim(task_name(3)) // ' ---'
  if (.not. eof) call read_int('Number of git commits: ', commits, eof)
  if (eof) then
    reason(3) = 'missing input'
  else if (commits >= 2) then
    task_score(3) = points
    reason(3) = 'original and commented versions committed'
  else
    reason(3) = 'need at least 2 commits'
  end if

  ! ---- Task 4: GitHub repository URL ----
  write(*, '(/, A)') '--- ' // trim(task_name(4)) // ' ---'
  if (.not. eof) call read_str('GitHub repository URL: ', url, eof)
  if (eof) then
    reason(4) = 'missing input'
  else
    call check_url(url, ok, reason(4))
    if (ok) task_score(4) = points
  end if

  ! ---- Report ----
  total = sum(task_score)
  write(*, '(/, A)') '=============== Report ==============='
  do t = 1, ntask
    write(*, '(A, T44, I3, A, I3, A, A)') trim(task_name(t)), task_score(t), ' /', points, &
         '  ', trim(merge('PASS: ', 'FAIL: ', task_score(t) > 0)) // ' ' // trim(reason(t))
  end do
  write(*, '(A)') '--------------------------------------------------'
  write(*, '(A, T44, I3, A, I3)') 'TOTAL', total, ' /', points * ntask

contains

  ! Prompt for an integer; re-prompt on invalid input; eof=.true. if input ends
  subroutine read_int(prompt, val, eof)
    character(len=*), intent(in) :: prompt
    integer(kind=8), intent(out) :: val
    logical, intent(out) :: eof
    character(len=64) :: line
    integer :: ios

    eof = .false.
    val = 0
    do
      write(*, '(A)', advance='no') prompt
      read(*, '(A)', iostat=ios) line
      if (ios /= 0) then
        write(*, '(A)') '(no input)'
        eof = .true.
        return
      end if
      if (len_trim(line) > 0 .and. verify(trim(adjustl(line)), '+-0123456789') == 0) then
        read(line, *, iostat=ios) val
        if (ios == 0) return
      end if
      write(*, '(A)') '  Please enter an integer.'
    end do
  end subroutine read_int

  ! Prompt for a non-empty string; eof=.true. if input ends
  subroutine read_str(prompt, val, eof)
    character(len=*), intent(in) :: prompt
    character(len=*), intent(out) :: val
    logical, intent(out) :: eof
    integer :: ios

    eof = .false.
    do
      write(*, '(A)', advance='no') prompt
      read(*, '(A)', iostat=ios) val
      if (ios /= 0) then
        write(*, '(A)') '(no input)'
        eof = .true.
        return
      end if
      val = adjustl(val)
      if (len_trim(val) > 0) return
      write(*, '(A)') '  Please enter a value.'
    end do
  end subroutine read_str

  ! Check that s equals 1+2+...+n
  subroutine check_sum(n, s, ok, msg)
    integer(kind=8), intent(in) :: n, s
    logical, intent(out) :: ok
    character(len=*), intent(out) :: msg
    integer(kind=8) :: expected

    ok = .false.
    if (n < 1 .or. n > nmax) then
      msg = 'N must be between 1 and 3000000000'
      return
    end if
    expected = n * (n + 1) / 2
    if (s == expected) then
      ok = .true.
      msg = 'output matches N*(N+1)/2'
    else
      write(msg, '(A, I0, A, I0)') 'output ', s, ' /= expected ', expected
    end if
  end subroutine check_sum

  ! Check that url looks like https://github.com/<owner>/<repo>[.git][/]
  subroutine check_url(url, ok, msg)
    character(len=*), intent(in) :: url
    logical, intent(out) :: ok
    character(len=*), intent(out) :: msg
    character(len=*), parameter :: prefix = 'https://github.com/'
    character(len=256) :: rest
    integer :: L, slash

    ok = .false.
    msg = 'URL must look like https://github.com/<owner>/<repo>'
    if (len_trim(url) <= len(prefix)) return
    if (url(1:len(prefix)) /= prefix) return
    if (index(trim(url), ' ') > 0) return

    rest = url(len(prefix) + 1:)
    L = len_trim(rest)
    if (rest(L:L) == '/') L = L - 1                         ! allow trailing slash
    if (L >= 4) then
      if (rest(L-3:L) == '.git') L = L - 4                  ! allow .git suffix
    end if
    if (L < 3) return

    slash = index(rest(1:L), '/')
    if (slash <= 1 .or. slash >= L) return                  ! need non-empty owner and repo
    if (index(rest(slash+1:L), '/') > 0) return             ! exactly owner/repo

    ok = .true.
    msg = 'valid GitHub repository URL'
  end subroutine check_url

end program grade_tasks
