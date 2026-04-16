program FFT_edge
    use consts
    use pgm_processing
    use FFTW_setup

    integer, allocatable            :: image(:,:)
    integer                         :: Nx, Ny
    integer                         :: cx, cy
    real(kind=dp)                   :: filter
    complex(kind=dp), allocatable   :: edge_image(:,:)
    real(kind=dp), allocatable      :: real_image(:,:)

    character(len=50)               :: filename

    write(*,*) "Please enter the name of the image file to be operated on"
    read(*,*) filename ! Would like to add some error checking if possible
    
    call read_pgm(trim(filename), image)
    !call write_pgm('clown2.pgm', image)
    Nx = size(image, 1)
    Ny = size(image, 2)
    ! print *, image
    ! print *, Nx, Ny
        ! edge_image should be half as wide+1, and equally as deep as image
    allocate(edge_image(int(Nx/2.0_dp)+1, Ny))
    allocate(real_image(Nx, Ny))
    real_image = real(image, kind=dp)

    call dfftw_plan_dft_r2c_2d(plan_forward, Nx, Ny, real_image, edge_image, &
    & FFTW_ESTIMATE)
    call dfftw_execute(plan_forward)
    call dfftw_destroy_plan(plan_forward)

    filter = 45
    do cx = 1, size(edge_image, 1)
        do cy = 1, size(edge_image, 2)
            if ((cx**2 + cy**2) .lt. filter **2)then
                edge_image(cx, cy) = cmplx(0.0_dp, 0.0_dp, kind=dp)
            end if
            if (((size(edge_image, 1) - cx)**2 + cy**2) .lt. filter **2) then
                edge_image(cx, cy) = cmplx(0.0_dp, 0.0_dp, kind=dp)
            end if
        end do 
    end do

    ! print*, edge_image

    call dfftw_plan_dft_c2r_2d(plan_backward, Nx, Ny, edge_image, real_image, &
    & FFTW_ESTIMATE)
    call dfftw_execute(plan_backward)
    call dfftw_destroy_plan(plan_backward)

    real_image = real_image/(Nx*Ny)
    call write_pgm('clown_edge.pgm', nint(abs(real_image)))
end program