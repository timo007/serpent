module crypto
use types, only : i1, i4, i8
implicit none
private
public sbox

contains

subroutine sbox(n, sbox_in, sbox_out)
    integer(kind=i4), intent(in)                :: n
    integer(kind=i8), dimension(4), intent(in)  :: sbox_in
    integer(kind=i8), dimension(4), intent(out) :: sbox_out

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

    sbox_out = 0_i8

    do word = 1, 4
        do bit = 0, 28, 4
            sbox_out(word) = ior(sbox_out(word), &
                 ishft(svals(ibits(sbox_in(word), bit, 4), n), bit))
        end do
    end do

end subroutine

end module
