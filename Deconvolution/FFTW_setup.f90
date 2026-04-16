module FFTW_setup
    implicit none
    integer, parameter :: fftw_forward=-1
    integer, parameter :: fftw_backward=+1
    integer, parameter :: fftw_destroy_input=1
    integer, parameter :: fftw_unaligned=2
    integer, parameter :: fftw_conserve_memory=4
    integer, parameter :: fftw_exhaustive=8
    integer, parameter :: fftw_preserve_input=16
    integer, parameter :: fftw_patient=32
    integer, parameter :: fftw_estimate=64

    integer, parameter :: i64=selected_int_kind(18) !decimal range 10 ^18
    integer(kind=i64) :: plan_forward, plan_backward
end