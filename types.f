module types
implicit none
private
public  i4, i8

integer, parameter  :: i4 = selected_int_kind(9)
integer, parameter  :: i8 = selected_int_kind(10)

end module
