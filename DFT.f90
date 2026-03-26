program DFT
    use consts

    implicit none

        ! Error checking
    integer :: ierr
    character(len = 50) :: err_msg = ''

        ! Timestep between samples
    real(kind = dp) :: dt

        ! Base value for transformation matrix
    complex(kind = dp)  :: W1
        ! Wave type to be made
    character(len = 50) :: waveform
        ! Wave frequency
    real(kind = dp) :: frequency
        ! Windowing type
    character(len = 50) :: windowing

        ! Length of arrays
    integer :: length
        ! Wave data storage
    complex(kind = dp), allocatable    :: h(:)
        ! Transformation matrix
    complex(kind = dp), allocatable    :: W(:,:)
        ! Output
    real(kind = dp), allocatable    :: amps(:)

        ! Counters for matrix initialisation loop
    integer :: c1, c2

        ! Open parameters file with error checking
    open(unit = 10, file = "params.txt", action = 'read', iostat = ierr, iomsg = err_msg)
    if (ierr .ne. 0) then
        write(*,*) "Error while opening parameter file"
        write(*,*) trim(err_msg)
        stop
    end if    
    
    read(10,*) length

    read(10,*) dt
    dt = 1.0_dp/dt

    read(10,*) waveform

    read(10,*) frequency

    read(10,*) windowing

    close(10)


    allocate(h(length))
    allocate(W(length, length))
    allocate(amps(length))

    select case(waveform)
        case ("sine")
            call sinewave(h, dt, frequency)
        case("triangle")
            call trianglewave(h, dt, frequency)
        case("square")
            call squarewave(h, dt, frequency)
        case default
            write(*,*) "Invalid waveform requested"
            stop
    end select

    call window(h, windowing)

    W1 = exp(2.0_dp*pi*i / real(length, kind = dp))
    c2 = 0

    do
        c1 = 0
        do
            W(c2+1, c1+1) = W1 **(c1 * c2)

            c1 = c1 + 1

            if (c1 .ge. (length)) exit
        end do
        c2 = c2 + 1

        if (c2 .ge. (length)) exit
    end do

    amps = abs(matmul(W, h))
    call write_out_freqs(amps, dt, 'output_freqs.txt')
    call write_out_wave(h, dt, 'output_wave.txt')

    contains

    subroutine sinewave(arr, dt, freq)
        complex(kind = dp)  :: arr(:)
        real(kind = dp) :: dt, freq
        integer :: count

        do count = 1, length
            arr(count) = sin((count * dt) * ((2.0_dp*pi)*freq))
        end do
    end subroutine

    subroutine trianglewave(arr, dt, freq)
        complex(kind = dp)  :: arr(:)
        real(kind = dp) :: dt, freq
        integer :: count

        do count = 1, length
            arr(count) = asin(sin((count * dt) * ((2.0_dp*pi)*freq)))
        end do
    end subroutine

    subroutine squarewave(arr, dt, freq)
        complex(kind = dp)  :: arr(:)
        real(kind = dp) :: dt, freq
        integer :: count

        do count = 1, length
            arr(count) = sin((count * dt) * ((2.0_dp*pi)*freq))
        end do

        where (real(arr) .gt. 0)
            arr = 1
        else where (real(arr) .lt. 0)
            arr = -1
        else where
            arr = 0
        end where
    end subroutine

    subroutine write_out_freqs(arr, dt, filename)
        real(kind = dp) :: arr(:), dt
        character(len=*), optional   :: filename
        
        integer :: ierr
        character(len = 50) :: errmsg = ''

        integer :: unit
        integer :: count

        unit = newunit()
        if (present(filename)) then
            open(unit = unit, file = filename, action = 'write', iostat = ierr, iomsg = errmsg)
            if(ierr .ne. 0) then
                write(*,*) "Error while opening output file" 
                write(*,*) trim(errmsg)
            end if
        else
            open(unit = unit, file = 'output.txt', action = 'write', iostat = ierr, iomsg = errmsg)
            if(ierr .ne. 0) then
                write(*,*) "Error while opening output file" 
                write(*,*) trim(errmsg)
            end if
        end if
       
        do count = 1, size(arr)
            write(unit,*) f(count, dt, size(arr)), arr(count)
        end do
    end subroutine

    subroutine write_out_wave(arr, dt, filename)
        complex(kind = dp) :: arr(:)
        real(kind = dp) :: dt
        character(len=*), optional   :: filename
        
        integer :: ierr
        character(len = 50) :: errmsg = ''

        integer :: unit
        integer :: count

        unit = newunit()
        if (present(filename)) then
            open(unit = unit, file = filename, action = 'write', iostat = ierr, iomsg = errmsg)
            if(ierr .ne. 0) then
                write(*,*) "Error while opening output file" 
                write(*,*) trim(errmsg)
            end if
        else
            open(unit = unit, file = 'output.txt', action = 'write', iostat = ierr, iomsg = errmsg)
            if(ierr .ne. 0) then
                write(*,*) "Error while opening output file" 
                write(*,*) trim(errmsg)
            end if
        end if
        
        do count = 1, size(arr)
            write(unit,*) dt*count, real(arr(count), kind = dp)
        end do

        close(unit)
    end subroutine

    function newunit(unit)

        integer, optional   :: unit
        integer             :: newunit
        logical             :: opened

            ! Start from given unit if supplied, else default to 10
        if (present(unit)) then
            newunit = unit
        else
            newunit = 10
        end if

        do
                ! Iterate through units
            newunit = newunit + 1
            inquire(unit = newunit, opened = opened)

                ! Leave loop if unit is unopened
            if (.not. opened) exit
                ! Leave loop if all units are occupied
            if (newunit .ge. 100) exit
        end do

            ! Return -1 if all options 10-99 are opened
        if (newunit .ge. 100) newunit = -1
    end function

    function f(n_int, dt, length_int) result(out)
        integer :: n_int, length_int
        real(kind = dp) :: n, length
        real(kind = dp) :: dt

        real(kind = dp) :: out

        length = real(length_int, kind = dp)
        n = real(n_int, kind = dp)
        
        out = n / (length * dt)
    end function

    subroutine window(arr, type)
        complex(kind = dp)  :: arr(:)
        character(len = 20) :: type

        integer :: count
        integer :: arr_len
        real(kind = dp) :: mult

        arr_len = size(arr, 1)

        select case(type)
        case("none")
            ! If no windowing, make no changes
        case("triangle")
            ! Centre has unmodified magnitude, then linear descent to 0 at edges
            do count = 1, arr_len
                mult = asin(sin(real(count, kind=dp) * pi / real(arr_len, kind=dp)))
                arr(count) = arr(count) * mult
            end do
        case("cosine")
            ! Half wavelength of cosine, with period modified to match data
            do count = 1, arr_len
                mult = cos((real(count, kind=dp) - real(arr_len/2.0_dp, kind=dp)) * pi / real(arr_len, kind=dp))
                arr(count) = arr(count) * mult
            end do
        case("gaussian")
            ! Uniform distribution where edges are close to 0 with broad middle and gentle descent
            do count = 1, arr_len
                mult = exp(-((count - (real(arr_len, kind=dp)/2.0_dp))**2)/(real(arr_len, kind=dp)*0.25_dp)**2)
                arr(count) = arr(count) * mult
            end do
        case default
            write(*,*) "Provided windowing type absent/invalid - use 'none' for no windowing" 
        end select
    end subroutine
end program