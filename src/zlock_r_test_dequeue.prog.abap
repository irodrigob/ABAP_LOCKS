*&---------------------------------------------------------------------*
*& Report ZLOCK_R_TEST_DEQUEUE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlock_r_test_dequeue.


PARAMETERS: p_appl   TYPE zlock_e_appl OBLIGATORY,
            p_subobj TYPE zlock_e_subobject.

SELECTION-SCREEN SKIP.
PARAMETERS: p_field1 TYPE zlock_e_field_name OBLIGATORY,
            p_value1 TYPE zlock_e_field_value OBLIGATORY.

PARAMETERS: p_field2 TYPE zlock_e_field_name,
            p_value2 TYPE zlock_e_field_value.

START-OF-SELECTION.

  DATA(lt_lock_values) = VALUE zcl_lock_management=>tt_lock_values( ( field = p_field1 value = p_value1 ) ).

  IF p_field2 IS NOT INITIAL.
    INSERT VALUE #( field = p_field2 value = p_value2 ) INTO TABLE lt_lock_values.
  ENDIF.

  TRY.
      NEW zcl_lock_management( )->dequeue( EXPORTING iv_appl        = p_appl
                                                     iv_subobject   = p_subobj
                                                     it_lock_values = lt_lock_values ).

      WRITE:/ 'Lock deleted.'.

    CATCH zcx_lock INTO DATA(lo_excep).
      WRITE:/ lo_excep->get_text( ).
  ENDTRY.
