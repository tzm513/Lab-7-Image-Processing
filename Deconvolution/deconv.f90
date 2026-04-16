program deconvolution
    use consts
    use pgm_processing
    use FFTW_setup

    integer, allocatable            :: image(:,:)
    integer                         :: Nx, Ny
    complex(kind=dp), allocatable   :: fft_image(:,:)
    real(kind=dp), allocatable      :: real_image(:,:)

    real(kind=dp), allocatable      :: convolution(:,:)
    complex(kind=dp), allocatable   :: fft_convolution(:,:)

    character(len=50)               :: filename

    write(*,*) "Please enter the name of the image file to be operated on"
    read(*,*) filename ! Would like to add some error checking if possible
    
    call read_pgm(trim(filename), image)

    Nx = size(image, 1)
    Ny = size(image, 2)

    allocate(convolution(Nx, Ny))
    convolution = 0

    convolution(10:30, 1) = 1
    !convolution((Nx-50):(Nx-30), 1) = 1

    !call write_pgm('clown2.pgm', image)
    ! print *, image
    ! print *, Nx, Ny
        ! edge_image should be half as wide+1, and equally as deep as image
    allocate(fft_image(int(Nx/2.0_dp)+1, Ny))
    allocate(fft_convolution(size(fft_image, 1), size(fft_image, 2)))
    allocate(real_image(Nx, Ny))
    real_image = real(image, kind=dp)

    call dfftw_plan_dft_r2c_2d(plan_forward, Nx, Ny, real_image, fft_image, &
    & FFTW_ESTIMATE)
    call dfftw_execute(plan_forward)
    
    call dfftw_plan_dft_r2c_2d(plan_forward, Nx, Ny, convolution, fft_convolution, &
    & FFTW_ESTIMATE)
    call dfftw_execute(plan_forward)
    
    call dfftw_destroy_plan(plan_forward)

    fft_image = fft_image/fft_convolution

    call dfftw_plan_dft_c2r_2d(plan_backward, Nx, Ny, fft_image, real_image, &
    & FFTW_ESTIMATE)
    call dfftw_execute(plan_backward)
    call dfftw_destroy_plan(plan_backward)

    real_image = real_image/(Nx*Ny)
    call write_pgm("unblurred_"//filename, nint(abs(real_image)))
end program