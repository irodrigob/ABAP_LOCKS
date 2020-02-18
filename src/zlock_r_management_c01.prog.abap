*&---------------------------------------------------------------------*
*&  Include           ZLOCK_R_MANAGEMENT_C01
*&---------------------------------------------------------------------*
CLASS lcl_alv_handler DEFINITION.
  PUBLIC SECTION.

    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
  PROTECTED SECTION.


ENDCLASS.
CLASS lcl_alv_handler IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM user_command USING e_salv_function.
  ENDMETHOD.



ENDCLASS.

CLASS lcl_controller DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS search_data
      IMPORTING
        !it_r_id        TYPE zcl_lock_management=>tt_r_id_lock OPTIONAL
        !it_r_appl      TYPE zcl_lock_management=>tt_r_appl OPTIONAL
        !it_r_subobject TYPE zcl_lock_management=>tt_r_subobject OPTIONAL
        !it_r_erdat     TYPE zcl_lock_management=>tt_r_erdat OPTIONAL
        !it_r_ernam     TYPE zcl_lock_management=>tt_r_ernam OPTIONAL
        !it_r_erzet     TYPE zcl_lock_management=>tt_r_erzet OPTIONAL.

    METHODS show_data.
    METHODS there_locks
      RETURNING VALUE(rv_there) TYPE sap_bool.
    METHODS on_user_command
      IMPORTING
        !iv_ucomm TYPE any.

  PROTECTED SECTION.
    TYPES: BEGIN OF ts_field_values_matrix,
             field1  TYPE zlock_t_0002-field,
             value1  TYPE zlock_t_0002-value,
             field2  TYPE zlock_t_0002-field,
             value2  TYPE zlock_t_0002-value,
             field3  TYPE zlock_t_0002-field,
             value3  TYPE zlock_t_0002-value,
             field4  TYPE zlock_t_0002-field,
             value4  TYPE zlock_t_0002-value,
             field5  TYPE zlock_t_0002-field,
             value5  TYPE zlock_t_0002-value,
             field6  TYPE zlock_t_0002-field,
             value6  TYPE zlock_t_0002-value,
             field7  TYPE zlock_t_0002-field,
             value7  TYPE zlock_t_0002-value,
             field8  TYPE zlock_t_0002-field,
             value8  TYPE zlock_t_0002-value,
             field9  TYPE zlock_t_0002-field,
             value9  TYPE zlock_t_0002-value,
             field10 TYPE zlock_t_0002-field,
             value10 TYPE zlock_t_0002-value,
           END OF ts_field_values_matrix.
    TYPES: BEGIN OF ts_alv_data.
        INCLUDE TYPE zcl_lock_management=>ts_query_header.
        INCLUDE TYPE ts_field_values_matrix.
    TYPES:
           END OF ts_alv_data.
    TYPES: tt_alv_data TYPE STANDARD TABLE OF ts_alv_data.

    DATA mt_alv_data TYPE tt_alv_data.
    DATA mo_mngt TYPE REF TO zcl_lock_management.
    DATA mv_max_field_lock TYPE i.
    DATA mo_alv TYPE REF TO cl_salv_table.
    DATA mo_alv_hanlder TYPE REF TO lcl_alv_handler.

    DATA mt_r_id        TYPE zcl_lock_management=>tt_r_id_lock.
    DATA mt_r_appl      TYPE zcl_lock_management=>tt_r_appl.
    DATA mt_r_subobject TYPE zcl_lock_management=>tt_r_subobject.
    DATA mt_r_erdat     TYPE zcl_lock_management=>tt_r_erdat.
    DATA mt_r_ernam     TYPE zcl_lock_management=>tt_r_ernam.
    DATA mt_r_erzet     TYPE zcl_lock_management=>tt_r_erzet.

    METHODS consolidate_values
      IMPORTING
        it_header_data TYPE zcl_lock_management=>tt_query_header
        it_values_data TYPE zcl_lock_management=>tt_query_values.
    METHODS adjust_alv_columns_fieldvalue.
    METHODS alv_refresh.
    METHODS delete_locks.



ENDCLASS.
CLASS lcl_controller IMPLEMENTATION.
  METHOD constructor.
    mo_mngt = NEW zcl_lock_management( ).
  ENDMETHOD.
  METHOD search_data.

    CLEAR mt_alv_data.

    " Se guarda los parámetros para poderlos volver a buscar
    mt_r_id = COND #( WHEN it_r_id IS SUPPLIED THEN it_r_id ELSE mt_r_id ).
    mt_r_appl = COND #( WHEN it_r_appl IS SUPPLIED THEN it_r_appl ELSE mt_r_appl ).
    mt_r_subobject = COND #( WHEN it_r_subobject IS SUPPLIED THEN it_r_subobject ELSE mt_r_subobject ).
    mt_r_erdat = COND #( WHEN it_r_erdat IS SUPPLIED THEN it_r_erdat ELSE mt_r_erdat ).
    mt_r_ernam = COND #( WHEN it_r_ernam IS SUPPLIED THEN it_r_ernam ELSE mt_r_ernam ).
    mt_r_erzet = COND #( WHEN it_r_erzet IS SUPPLIED THEN it_r_erzet ELSE mt_r_erzet )..

    " Búsqueda de los bloqueos
    mo_mngt->query(
      EXPORTING
        it_r_id        = mt_r_id
        it_r_appl      = mt_r_appl
        it_r_subobject = mt_r_subobject
        it_r_erdat     = mt_r_erdat
        it_r_ernam     = mt_r_ernam
        it_r_erzet     = mt_r_erzet
      IMPORTING
        et_header_data = DATA(lt_header_data)
        et_values_data = DATA(lt_values_data) ).

    " Los datos obtenidos se consolidan en la tabla de datos que se mostrará
    consolidate_values( EXPORTING it_header_data = lt_header_data
                                  it_values_data = lt_values_data ).

  ENDMETHOD.

  METHOD consolidate_values.
    LOOP AT it_header_data ASSIGNING FIELD-SYMBOL(<ls_header_data>).
      DATA(ls_data) = CORRESPONDING ts_alv_data( <ls_header_data> ).

      " Se pasan los valores del bloqueo a los campos fijos del listado ALV
      DATA(lv_cont) = 1.
      LOOP AT it_values_data ASSIGNING FIELD-SYMBOL(<ls_values_data>) WHERE id = <ls_header_data>-id.

        mv_max_field_lock = COND #( WHEN lv_cont > mv_max_field_lock THEN lv_cont ELSE mv_max_field_lock ).

        DATA(lv_field) = |FIELD{ lv_cont }|.
        ASSIGN COMPONENT lv_field OF STRUCTURE ls_data TO FIELD-SYMBOL(<field>).
        IF sy-subrc = 0.
          <field> = <ls_values_data>-field.

          lv_field = |VALUE{ lv_cont }|.
          ASSIGN COMPONENT lv_field OF STRUCTURE ls_data TO <field>.
          IF sy-subrc = 0.
            <field> = <ls_values_data>-value.
          ENDIF.

        ELSE. " Si no existe es que hay más valores que campos
          EXIT.
        ENDIF.

        lv_cont = lv_cont + 1.
      ENDLOOP.

      INSERT ls_data INTO TABLE mt_alv_data.

    ENDLOOP.
  ENDMETHOD.

  METHOD show_data.
    TRY.
        cl_salv_table=>factory( EXPORTING list_display = abap_false
                                IMPORTING r_salv_table = mo_alv
                                CHANGING t_table      = mt_alv_data ).

        " Listado del ALV
        DATA(lo_settings) = mo_alv->get_display_settings(  ).
        lo_settings->set_list_header( sy-title ).

        " Layout para poder grabar variantes
        DATA(lo_layout) = mo_alv->get_layout(  ).
        lo_layout->set_key( value = VALUE #( report = sy-repid ) ).

        " Funciones estándar
        DATA(lo_functions) = mo_alv->get_functions(  ).

        " Modo de selección
        DATA(lo_selections) = mo_alv->get_selections(  ).
        lo_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

        " Status de pantalla
        mo_alv->set_screen_status( pfstatus      =  'STANDARD'
                                   report        =  sy-repid
                                   set_functions = mo_alv->c_functions_all ).

        " Ajuste de las columnas de campo valor del bloqueo
        adjust_alv_columns_fieldvalue(  ).

        " Columnas optimizadas
        DATA(lo_columns) = mo_alv->get_columns(  ).
        lo_columns->set_optimize( abap_true ).

        " Eventos del listado
        mo_alv_hanlder = NEW lcl_alv_handler( ).

        DATA(lo_events) = mo_alv->get_event(  ).
        SET HANDLER mo_alv_hanlder->on_user_command FOR lo_events.


        " Se muestra el listado
        mo_alv->display(  ).


      CATCH cx_salv_msg.
    ENDTRY.
  ENDMETHOD.


  METHOD adjust_alv_columns_fieldvalue.
    DATA(lo_columns) = mo_alv->get_columns(  ).

    " Se ajusta las 10 columnas donde se indica el campo clave/valor para ponerles un texto distinto.
    " Si hay menos campos valor que columnas configuradas se quitarán del listado
    DO.
      DATA(lv_index) = sy-index.
      TRY.

          " Campo del nombre
          " Si el numero actual de campo supera al máximo que hay de campos de valores, el campo se marca como técnico para que no se muestre
          DATA(lo_column) = lo_columns->get_column( CONV #( |FIELD{ lv_index }| ) ).
          IF  lv_index <= mv_max_field_lock.
            " Se informa el titulo del campos
            DATA(lv_text) = |{ TEXT-c01 } { lv_index }|.
            lo_column->set_short_text( CONV #( lv_text ) ).
            lo_column->set_medium_text( CONV #( lv_text ) ).
            lo_column->set_long_text( CONV #( lv_text ) ).
            lo_column->set_optimized( abap_true ).

          ELSE.
            lo_column->set_technical( abap_true ).
          ENDIF.

          " Campo del valor, se aplica lo mismo que para el nombre
          lo_column = lo_columns->get_column( CONV #( |VALUE{ lv_index }| ) ).
          IF lv_index <= mv_max_field_lock.

            " Se informa el titulo del campos
            lv_text = |{ TEXT-c02 } { lv_index }|.
            lo_column->set_short_text( CONV #( lv_text ) ).
            lo_column->set_medium_text( CONV #( lv_text ) ).
            lo_column->set_long_text( CONV #( lv_text ) ).
            lo_column->set_optimized( abap_true ).

          ELSE.
            lo_column->set_technical( abap_true ).
          ENDIF.

        CATCH cx_salv_not_found . " Si el campo se sale del proceso
          EXIT.
      ENDTRY.
    ENDDO.

  ENDMETHOD.

  METHOD there_locks.
    rv_there = COND #( WHEN mt_alv_data IS NOT INITIAL THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD on_user_command.
    CASE iv_ucomm.
      WHEN 'REFRESH'.
        alv_refresh( ).
      WHEN 'UNLOCK'.
        delete_locks(  ).
    ENDCASE.
  ENDMETHOD.


  METHOD alv_refresh.
    " El refresco consiste en volver a buscar los datos y actualizar el listado
    search_data( ).

    mo_alv->refresh(  ).

  ENDMETHOD.


  METHOD delete_locks.

    " Se leen las filas seccionadas
    DATA(lt_rows) = mo_alv->get_selections(  )->get_selected_rows(  ).

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<ls_rows>).

      READ TABLE mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX <ls_rows>.
      IF sy-subrc = 0.
        TRY.
            mo_mngt->dequeue_by_id( iv_commit = abap_true
                                    iv_id = <ls_data>-id ).

            DELETE mt_alv_data INDEX <ls_rows>.

          CATCH zcx_lock.
        ENDTRY.
      ENDIF.

    ENDLOOP.
    IF sy-subrc NE 0.
      MESSAGE s005(zlock).
    ELSE.
      mo_alv->refresh(  ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
