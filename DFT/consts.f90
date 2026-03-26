module consts
    implicit none
    integer, parameter  :: dp = selected_real_kind(15, 300)

    real(kind = dp), parameter  :: pi = 3.1415926
    complex(kind = dp), parameter   :: i = cmplx(0.0_dp, 1.0_dp, kind=dp)
    
end module