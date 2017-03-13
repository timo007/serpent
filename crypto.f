module crypto
use types, only : i1, i4, i8
implicit none
private
public sbox

contains

subroutine key_mix(input, subkey, output)
    !
    ! Mix 128 bits of input_data (input as four 32 bit words) with a 128 bit sub-key
    ! (also input as four 32 bit words)
    !
    integer(kind=i8), dimension(4), intent(in)  :: input
    integer(kind=i8), dimension(4), intent(in)  :: subkey
    integer(kind=i8), dimension(4), intent(out) :: output

    integer(kind=i4)                            :: word

    do word = 1, 4
        output(word) = ieor(input(word), subkey(word))
    end do

end subroutine
    

subroutine sbox(n, input, output)
    !
    ! Serpent uses eight S-Boxes (n=0, ... 8). Each S-Box takes a four bit input and
    ! outputs a four bit number. Each round of serpent applies an S-Box to 128 bits
    ! which are input and output as four 32 bit words.
    !
    ! This code is not optimised.
    !
    integer(kind=i4), intent(in)                :: n
    integer(kind=i8), dimension(4), intent(in)  :: input
    integer(kind=i8), dimension(4), intent(out) :: output

    integer(kind=i1), dimension(0:15, 0:7)      :: svals
    integer(kind=i4)                            :: word, bit

    svals = reshape((/   3,  8, 15,  1, 10,  6,  5, 11, 14, 13,  4,  2,  7,  0,  9, 12, &
                        15, 12,  2,  7,  9,  0,  5, 10,  1, 11, 14,  8,  6, 13,  3, 4,  &
                         8,  6,  7,  9,  3, 12, 10, 15, 13,  1, 14,  4,  0, 11,  5, 2,  &
                         0, 15, 11,  8, 12,  9,  6,  3, 13,  1,  2,  4, 10,  7,  5, 14, &
                         1, 15,  8,  3, 12,  0, 11,  6,  2,  5,  4, 10,  9, 14,  7, 13, &
                        15,  5,  2, 11,  4, 10,  9, 12,  0,  3, 14,  8, 13,  6,  7, 1,  &
                         7,  2, 12,  5,  8,  4,  6, 11, 14,  9,  1, 15, 13,  3, 10, 0,  &
                         1, 13, 15,  0, 14,  8,  2, 11,  7,  4, 12, 10,  9,  3,  5, 6 /), &
                        shape(svals))

    output = 0_i8

    do word = 1, 4
        do bit = 0, 28, 4
            output(word) = ior(output(word), ishft(svals(ibits(input(word), bit, 4), n), bit))
        end do
    end do

end subroutine


subroutine linear_transfor(input)
    !
    ! Linearly transform the output from the S-Boxes.
    !
    integer(kind=i8), dimension(4), intent(inout)   :: input

    input(1) = ishftc(input(1), 13, 32)
    input(3) = ishftc(input(2), 3, 32)
    input(2) = ieor(ieor(input(2), input(1)), input(3))
    input(4) = ieor(ieor(input(4), input(3)), ishft(input(1), 3))
    input(2) = ishftc(input(2), 1, 32)
    input(4) = ishftc(input(4), 7, 32)
    input(1) = ieor(ieor(input(1), input(2)), input(4))
    input(3) = ieor(ieor(input(3), input(4)), ishft(input(2), 7))
    input(1) = ishftc(input(1), 5, 32)
    input(3) = ishftc(input(3), 22, 32)

end subroutine

subroutine pad_key(key)
    !
    ! Pad the key out to 256 bits, if necessary.
    !
end subroutine

end module
