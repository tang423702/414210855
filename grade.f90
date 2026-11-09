! Task achievement grading system for HW1 (Tasks 1-4)
! Each task is worth 20 points (80 total); answer Y if the task was achieved.
program grade_tasks
  implicit none
  integer, parameter :: ntask = 4, points = 20
  integer :: task_score(ntask), total, t
  character(len=40) :: task_name(ntask)
  character(len=80) :: question(ntask)

  task_name(1) = 'Task 1: Write, compile and run program'
  task_name(2) = 'Task 2: Three random verifications'
  task_name(3) = 'Task 3: Git version control'
  task_name(4) = 'Task 4: Push to GitHub'

  question(1) = 'Sum 1..N program written, compiled with gfortran, output correct?'
  question(2) = 'All three random tests match N*(N+1)/2?'
  question(3) = 'Original committed, commented version verified and committed?'
  question(4) = 'Repository pushed to GitHub successfully?'

  task_score = 0

  write(*, '(A)') '===== HW1 Task Achievement Grading ====='
  write(*, '(A)') 'Answer each question with Y (achieved) or N (not achieved).'

  do t = 1, ntask
    write(*, '(/, A)') '--- ' // trim(task_name(t)) // ' ---'
    if (ask(trim(question(t)))) task_score(t) = points
  end do

  ! ---- Report ----
  total = sum(task_score)
  write(*, '(/, A)') '=============== Report ==============='
  do t = 1, ntask
    write(*, '(A, T44, I3, A, I3)') trim(task_name(t)), task_score(t), ' /', points
  end do
  write(*, '(A)') '--------------------------------------------------'
  write(*, '(A, T44, I3, A, I3)') 'TOTAL', total, ' /', points * ntask

contains

  ! Ask one Y/N question; return .true. if answered Y
  logical function ask(q)
    character(len=*), intent(in) :: q
    character(len=16) :: ans
    integer :: ios

    ask = .false.
    do
      write(*, '(A, A, I0, A)', advance='no') q, ' (', points, ' pts) [Y/N]: '
      read(*, '(A)', iostat=ios) ans
      if (ios /= 0) then                       ! end of input: treat as not achieved
        write(*, '(A)') 'N (no input)'
        return
      end if
      ans = adjustl(ans)
      select case (ans(1:1))
      case ('Y', 'y')
        ask = .true.
        return
      case ('N', 'n')
        return
      case default
        write(*, '(A)') '  Please enter Y or N.'
      end select
    end do
  end function ask

end program grade_tasks
