*&---------------------------------------------------------------------*
*&  Include           ZLOCK_R_MANAGEMENT_F01
*&---------------------------------------------------------------------*

FORM user_command USING pe_ucomm.
  mo_controller->on_user_command( pe_ucomm ).
ENDFORM.
