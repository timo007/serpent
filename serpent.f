program serpent
use types, only:    i8
use crypto, only:   sbox
implicit none

integer(kind=i8), dimension(4)  :: sbox_in
integer(kind=i8), dimension(4)  :: sbox_out

sbox_in = (/ z'01234567', z'89ABCDEF', z'FEDCBA98', z'76543210' /)

call sbox(1, sbox_in, sbox_out)

!write (*, '(4(4Z9.8/))') block(1:16)

end program serpent
