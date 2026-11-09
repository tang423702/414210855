program sum_to_n                                       ! 主程式開始，名稱為 sum_to_n
  implicit none                                        ! 關閉隱式型別宣告，所有變數都必須明確宣告
  integer(kind=8) :: n, i, total                       ! 宣告 64 位元整數：n 為上限、i 為迴圈計數、total 為累加和

  write(*, '(A)', advance='no') 'Enter N: '            ! 輸出提示文字，且不換行，讓使用者在同一行輸入
  read(*, *) n                                         ! 從標準輸入讀取 N 的值

  if (n < 1) then                                      ! 檢查 N 是否小於 1（不合法的輸入）
    write(*, '(A)') 'N must be a positive integer.'    ! 輸出錯誤訊息
    stop                                               ! 終止程式執行
  end if                                               ! 結束 if 判斷

  total = 0                                            ! 將累加和初始化為 0
  do i = 1, n                                          ! 迴圈：i 從 1 跑到 n
    total = total + i                                  ! 將目前的 i 加進累加和
  end do                                               ! 迴圈結束

  write(*, '(A, I0, A, I0)') 'Sum of 1 to ', n, ' = ', total   ! 輸出迴圈計算的結果（I0 表示以最小寬度輸出整數）
  write(*, '(A, I0)') 'Check (N*(N+1)/2) = ', n * (n + 1) / 2  ! 用公式 N(N+1)/2 計算並輸出，以便對照驗證
end program sum_to_n                                   ! 主程式結束
