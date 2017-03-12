module crypto
use types, only : i4, i8
implicit none
private
!public chacha20_block, quarter_round, inner_block

contains

subroutine sbox(n, inbox, outbox)
    integer(kind=i4), intent(in)                :: n
    integer(kind=i8), dimension(4), intent(in)  :: inbox
    integer(kind=i8), dimension(4), intent(out) :: outbox

    integer(kind=i8), dimension(5)              :: x = 0_i8

    x(1:4) = inbox(1:4)

    select case (n)
        case (0)
            x(5) = x(4);   
            x(4) =  ior(x(4), x(1)); x(1) = ieor(x(1), x(5)); x(5) = ieor(x(5), x(3));   
            x(5) =  not(x(5))      ; x(4) = ieor(x(4), x(2)); x(2) = iand(x(2), x(1));   
            x(2) = ieor(x(2), x(5)); x(3) = ieor(x(3), x(1)); x(1) = ieor(x(1), x(4));   
            x(5) =  ior(x(5), x(1)); x(1) = ieor(x(1), x(3)); x(3) = iand(x(3), x(2));   
            x(4) = ieor(x(4), x(3)); x(2) =  not(x(2))      ; x(3) = ieor(x(3), x(5));   
            x(2) = ieor(x(2), x(3));

        case (1)
            x(5) = x(2);   
            x(2) = ieor(x(2), x(1)); x(1) = ieor(x(1), x(4)); x(4) =  not(x(4));   
            x(5) = iand(x(5), x(2)); x(1) =  ior(x(1), x(2)); x(4) = ieor(x(4), x(3));   
            x(1) = ieor(x(1), x(4)); x(2) = ieor(x(2), x(4)); x(4) = ieor(x(4), x(5));   
            x(2) =  ior(x(2), x(5)); x(5) = ieor(x(5), x(3)); x(3) = iand(x(3), x(1));   
            x(3) = ieor(x(3), x(2)); x(2) =  ior(x(2), x(1)); x(1) =  not(x(1));   
            x(1) = ieor(x(1), x(3)); x(5) = ieor(x(5), x(2));

        case (2)
            x(4) =  not(x(4));   
            x(2) = ieor(x(2), x(1)); x(5) = x(1)            ; x(1) = iand(x(1), x(3));   
            x(1) = ieor(x(1), x(4)); x(4) =  ior(x(4), x(5)); x(3) = ieor(x(3), x(2));   
            x(4) = ieor(x(4), x(2)); x(2) = iand(x(2), x(1)); x(1) = ieor(x(1), x(3));   
            x(3) = iand(x(3), x(4)); x(4) =  ior(x(4), x(2)); x(1) =  not(x(1));   
            x(4) = ieor(x(4), x(1)); x(5) = ieor(x(5), x(1)); x(1) = ieor(x(1), x(3));   
            x(2) =  ior(x(2), x(3));

        case (3)
            x(5) = x(2);   
            x(2) = ieor(x(2), x(4)); x(4) =  ior(x(4), x(1)); x(5) = iand(x(5), x(1));   
            x(1) = ieor(x(1), x(3)); x(3) = ieor(x(3), x(2)); x(2) = iand(x(2), x(4));   
            x(3) = ieor(x(3), x(4)); x(1) =  ior(x(1), x(5)); x(5) = ieor(x(5), x(4));   
            x(2) = ieor(x(2), x(1)); x(1) = iand(x(1), x(4)); x(4) = iand(x(4), x(5));   
            x(4) = ieor(x(4), x(3)); x(5) =  ior(x(5), x(2)); x(3) = iand(x(3), x(2));   
            x(5) = ieor(x(5), x(4)); x(1) = ieor(x(1), x(4)); x(4) = ieor(x(4), x(3));

        case (4)
            x(5) = x(4);   
            x(4) = iand(x(4), x(1)); x(1) = ieor(x(1), x(5));           
            x(4) = ieor(x(4), x(3)); x(3) =  ior(x(3), x(5)); x(1) = ieor(x(1), x(2));   
            x(5) = ieor(x(5), x(4)); x(3) =  ior(x(3), x(1));           
            x(3) = ieor(x(3), x(2)); x(2) = iand(x(2), x(1));           
            x(2) = ieor(x(2), x(5)); x(5) = iand(x(5), x(3)); x(3) = ieor(x(3), x(4));   
            x(5) = ieor(x(5), x(1)); x(4) =  ior(x(4), x(2)); x(2) =  not(x(2));   
            x(4) = ieor(x(4), x(1));

        case (5)
            x(5) = x(2)            ; x(2) =  ior(x(2), x(1));           
            x(3) = ieor(x(3), x(2)); x(4) =  not(x(4));       x(5) = ieor(x(5), x(1));   
            x(1) = ieor(x(1), x(3)); x(2) = iand(x(2), x(5)); x(5) =  ior(x(5), x(4));   
            x(5) = ieor(x(5), x(1)); x(1) = iand(x(1), x(4)); x(2) = ieor(x(2), x(4));   
            x(4) = ieor(x(4), x(3)); x(1) = ieor(x(1), x(2)); x(3) = iand(x(3), x(5));   
            x(2) = ieor(x(2), x(3)); x(3) = iand(x(3), x(1));           
            x(4) = ieor(x(4), x(3));

        case (6)
            x(5) = x(2);   
            x(4) = ieor(x(4), x(1)); x(2) = ieor(x(2), x(3)); x(3) = ieor(x(3), x(1));   
            x(1) = iand(x(1), x(4)); x(2) =  ior(x(2), x(4)); x(5) = not(x(5));   
            x(1) = ieor(x(1), x(2)); x(2) = ieor(x(2), x(3));           
            x(4) = ieor(x(4), x(5)); x(5) = ieor(x(5), x(1)); x(3) = iand(x(3), x(1));   
            x(5) = ieor(x(5), x(2)); x(3) = ieor(x(3), x(4)); x(4) = iand(x(4), x(2));   
            x(4) = ieor(x(4), x(1)); x(2) = ieor(x(2), x(3));

        case (7)
            x(2) = not(x(2));   
            x(5) = x(2)            ; x(1) =  not(x(1))      ; x(2) = iand(x(2), x(3));   
            x(2) = ieor(x(2), x(4)); x(4) =  ior(x(4), x(5)); x(5) = ieor(x(5), x(3));   
            x(3) = ieor(x(3), x(4)); x(4) = ieor(x(4), x(1)); x(1) =  ior(x(1), x(2));   
            x(3) = iand(x(3), x(1)); x(1) = ieor(x(1), x(5)); x(5) = ieor(x(5), x(4));   
            x(4) = iand(x(4), x(1)); x(5) = ieor(x(5), x(2));           
            x(3) = ieor(x(3), x(5)); x(4) = ieor(x(4), x(2)); x(5) =  ior(x(5), x(1);   
            x(5) = ieor(x(5), x(2));

    end select
        


    
end subroutine

end module
