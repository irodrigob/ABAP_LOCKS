*&---------------------------------------------------------------------*
*& Report ZLOCK_R_MANAGEMENT
*&---------------------------------------------------------------------*
*& Description: Lock management
*&---------------------------------------------------------------------*
REPORT zlock_r_management MESSAGE-ID zlock.

INCLUDE zlock_r_management_top.
INCLUDE zlock_r_management_c01.

*----------------------------------------------------------------------*
*  Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: s_id FOR zlock_t_0001-id,
                s_appl FOR zlock_t_0001-appl,
                s_subobj FOR zlock_t_0001-subobject,
                s_erdat FOR zlock_t_0001-erdat,
                s_erzet FOR zlock_t_0001-erzet,
                s_ernam FOR zlock_t_0001-ernam.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*  Selection data
*----------------------------------------------------------------------*
START-OF-SELECTION.

  mo_controller = NEW lcl_controller( ).

  mo_controller->search_data(
    EXPORTING
      it_r_id        = s_id[]
      it_r_appl      = s_appl[]
      it_r_subobject = s_subobj[]
      it_r_erdat      = s_erdat[]
      it_r_ernam     = s_ernam[]
      it_r_erzet     = s_erzet[]   ).

*----------------------------------------------------------------------*
*  End of selection data
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF mo_controller->there_locks(  ) = abap_true.
    mo_controller->show_data(  ).
  ELSE.
    MESSAGE s004.
  ENDIF.

  INCLUDE zlock_r_management_f01.
