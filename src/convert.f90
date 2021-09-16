!> convert.f90
!> Defines subroutines for converting units

!> convert from meter to feet
real function m_to_ft(value)
    implicit none

    real, intent(in):: value

    m_to_ft = value * (100. / 30.48)
end function

!> convert from centi-meter to feet
real function cm_to_ft(value)
    implicit none

    real, intent(in):: value

    cm_to_ft = value / 30.48
end function

!> convert from centi-meter to inch
real function cm_to_inch(value)
    implicit none

    real, intent(in):: value

    cm_to_inch = value / 2.54
end function

!> convert from square-meter to square-feet
real function m2_to_ft2(value)
    implicit none

    real, intent(in):: value

    m2_to_ft2 = value * (100. / 30.48) * (100. / 30.48)
end function

!> convert from cubic-meter to cubit-feet
real function m3_to_ft3(value)
    implicit none

    real, intent(in):: value

    m3_to_ft3 = value * (100. / 30.48) * (100. / 30.48) * (100. / 30.48)
end function

!> convert array values from meter to feet
subroutine m_to_ft_array(size, values)
    implicit none

    integer, intent(in):: size
    real, intent(inout):: values(size)
    real:: m_to_ft

    integer:: i

    do i = 1, size
        values(i) = m_to_ft(values(i))
    end do
end subroutine

!> convert array values from centi-meter to feet
subroutine cm_to_ft_array(size, values)
    implicit none

    integer, intent(in):: size
    real, intent(inout):: values(size)
    real:: cm_to_ft

    integer:: i

    do i = 1, size
        values(i) = cm_to_ft(values(i))
    end do
end subroutine

!> convert array values from centi-meter to inch
subroutine cm_to_inch_array(size, values)
    implicit none

    integer, intent(in):: size
    real, intent(inout):: values(size)
    real:: cm_to_inch

    integer:: i

    do i = 1, size
        values(i) = cm_to_inch(values(i))
    end do
end subroutine

!> convert array values from centi-meter to feet
subroutine m3_to_ft3_array(size, values)
    implicit none

    integer, intent(in):: size
    real, intent(inout):: values(size)
    real:: m3_to_ft3

    integer:: i

    do i = 1, size
        values(i) = m3_to_ft3(values(i))
    end do
end subroutine

!> convert from inch to centimeter
real function inch_to_cm(value)
    implicit none

    real, intent(in):: value

    inch_to_cm = value * 2.54
end function

!> convert from feet to meter
real function ft_to_m(value)
    implicit none

    real, intent(in):: value

    ft_to_m = value * 30.48 / 100.
end function

!> convert from square-feet to square-meter
real function ft2_to_m2(value)
    implicit none
 
    real, intent(in):: value

    ft2_to_m2 = value * (30.48 / 100.) * (30.48 / 100.)
end function

!> convert from cubic-feet to cubic-meter
real function ft3_to_m3(value)
    implicit none
 
    real, intent(in):: value

    ft3_to_m3 = value * (30.48 / 100.) * (30.48 / 100.) * (30.48 / 100.)
end function
