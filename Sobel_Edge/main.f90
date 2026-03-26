program Edge_Detection
    use consts
    use pgm_processing

    integer, allocatable    :: image(:,:)
    integer                 :: count_x, count_y
    integer, allocatable    :: out_image(:,:)
    integer                 :: out_x, out_y


    call read_pgm("clown.pgm", image)
    allocate(out_image(size(image, 1)-2, size(image, 2)-2))

    do count_x = 2, size(image, 1)-1
        do count_y = 2, size(image, 2)-1
            out_x = sobel(image(count_x-1:count_x+1, count_y-1:count_y+1), 'x')
            out_y = sobel(image(count_x-1:count_x+1, count_y-1:count_y+1), 'y')
            out_image(count_x-1, count_y-1) = nint(sqrt(real(out_x**2 + out_y**2, kind=dp)))
        end do
    end do

    !count_x = 14
    !count_y = 227
    !
    !out_x = sobel(image(count_x-1:count_x+1, count_y-1:count_y+1), 'x')
    !write(*,*) "out_x: ", out_x
    !out_y = sobel(image(count_x-1:count_x+1, count_y-1:count_y+1), 'x')
    !write(*,*) "out_y: ", out_y
    !out_image(count_x-1, count_y-1) = int((out_x**2 + out_y**2)**0.5)
    !write(*,*) "out_image: ", out_image(count_x-1, count_y-1)

    call write_pgm("clown_grad.pgm", out_image)

    contains

    function sobel(arr, direction) result(s)
        integer :: arr(:, :)
        character(len=*) :: direction
        integer :: G(3, 3)
        integer :: ky, kx
        integer :: s

        select case(direction)
        case('x')
            G = reshape((/-1, 0, 1,&
                          -2, 0, 2,&
                          -1, 0, 1/), (/3, 3/))
        case('y')
            G = reshape((/-1,-2,-1,&
                           0, 0, 0,&
                           1, 2, 1/), (/3, 3/))
        case default
            write(*,*) "Invalid direction provided for Sobel filter."
        end select

        s = 0
        do kx = 1,3
            do ky = 1,3
                s = s + (arr(kx,ky) * G(kx,ky))
            end do
        end do
    end function
end program