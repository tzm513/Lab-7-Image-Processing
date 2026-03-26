module pgm_processing

    implicit none
    integer, parameter  :: dp = selected_real_kind(15, 300)
    private             :: dp

    contains

    !> @brief Finds the next unopened unit
    !!
    !! Defaults to starting from 10 but will start from 'unit' if supplied
    !! Returns -1 if there are no unopened units 10-99
    !!
    !! # Parameters:
    !! integer, optional    :: unit     ! Last known opened unit
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

        return
    end function


    !> @brief Looks for given file and, if present, reads data into array
    !!
    !! # Parameters:
    !! character (len=*)    :: filename     ! Name of file to search for
    !! integer              :: array(:,:)   ! Storage space for pgm data
    subroutine read_pgm(filename, array)

            ! Input vars
        character (len=*)       :: filename 
        integer, allocatable    :: array(:, :)

            ! File utility
        integer                 :: u, ierr
        character (len = 50)    :: errmsg

            ! PGM parameters
        integer :: Nx, Ny
        integer :: max_greys

            ! Loop counters
        integer :: i, j, k

            ! Format flags
        11 format(a2)
        12 format(i4,i4)
        13 format(i5)
        14 format(17(i3.3, 1x))

            ! Ensure array is not allocated
        if (allocated(array)) deallocate(array)

            ! Open target file in a new unit, and return error code if necessary
        u = newunit()
        open(unit=u, file=filename, action='read', iostat=ierr, iomsg=errmsg)
        if (.not. ierr .eq. 0) write(*,*) trim(errmsg)

        read(u,11)  ! skip pgm 'magic number' line

        Nx = ubound(array, 1) - lbound(array, 1) + 1
        Ny = ubound(array, 2) - lbound(array, 2) + 1
        read(u,12) Nx,Ny !width,˜height˜
        write(*,*) Nx, Ny
        allocate(array(Nx, Ny))

        read(u,13) max_greys !max˜gray˜value˜
    

        do j=lbound(array, 2), ubound(array, 2)
            do i=lbound(array, 1), ubound(array, 1) - 17, 17
                read(u,*) (array(i + k, j), k = 0, 16) !line less than 70 cha
            end do
            read(u, 14) (array(k, j), k = i, ubound(array, 1))
        end do
    end subroutine

    !> @brief Writes input array data to .pgm format
    !!
    !! # Parameters
    !! character (len=*)    :: filename
    !! integer              :: array(:,:)
    subroutine write_pgm(filename, array)

            ! Input vars
        character (len=*)   :: filename
        integer             :: array (:,:)

            ! File utility
        integer             :: u, ierr
        character(len=50)   :: errmsg

            ! Array data
        integer             :: Nx, Ny, max_greys
        integer :: i, j, k

            ! Format flags
        11 format(a2)
        12 format(i4,1x,i4)
        13 format(i5)
        14 format(17(i3.3, 1x))

            ! Open (or create) target file and return error message if necessary
        u = newunit()
        open(unit=u,file=filename, action='write', iostat=ierr, iomsg=errmsg)
        if (.not. ierr .eq. 0) write(*,*) trim(errmsg)

        write(u,11) 'P2 '    ! pgm 'magic number'

        Nx = ubound(array, 1) - lbound(array, 1) + 1
        Ny = ubound(array, 2) - lbound(array, 2) + 1
        write(u,12) Nx,Ny !width,˜height˜

        max_greys = maxval(array)
        write(u,13) max_greys !max˜gray˜value˜
    

        do j=lbound(array, 2), ubound(array, 2)
            do i=lbound(array, 1), ubound(array, 1) - 17, 17
                write(u, 14) (array(i + k, j), k = 0, 16) !line less than 70 cha
            end do
            write(u, 14) (array(k, j), k = i, ubound(array, 1))
        end do
    end subroutine
end module