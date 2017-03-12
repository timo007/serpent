module types
implicit none
private
public  i1, i4, i8

integer, parameter  :: i1 = selected_int_kind(2)
integer, parameter  :: i2 = selected_int_kind(4)
integer, parameter  :: i4 = selected_int_kind(9)
integer, parameter  :: i8 = selected_int_kind(10)

end module
