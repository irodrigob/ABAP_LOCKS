class ZCL_LOCK_MANAGEMENT definition
  public
  final
  create public .

public section.

  types:
    tt_r_id_lock TYPE RANGE OF zlock_t_0001-id .
  types:
    tt_r_subobject TYPE RANGE OF zlock_t_0001-subobject .
  types:
    tt_r_appl TYPE RANGE OF zlock_t_0001-appl .
  types:
    tt_r_erdat TYPE RANGE OF zlock_t_0001-erdat .
  types:
    tt_r_ernam TYPE RANGE OF zlock_t_0001-ernam .
  types:
    tt_r_erzet TYPE RANGE OF zlock_t_0001-erzet .
  types:
    BEGIN OF ts_lock_values,
             field TYPE zlock_e_field_name,
             value TYPE zlock_e_field_value,
           END OF ts_lock_values .
  types:
    tt_lock_values TYPE STANDARD TABLE OF ts_lock_values WITH EMPTY KEY .
  types:
    BEGIN OF ts_query_header,
             id        TYPE zlock_t_0001-id,
             appl      TYPE zlock_t_0001-appl,
             subobject TYPE zlock_t_0001-subobject,
             erdat     TYPE zlock_t_0001-erdat,
             erzet     TYPE zlock_t_0001-erzet,
             ernam     TYPE zlock_t_0001-ernam,
           END OF ts_query_header .
  types:
    tt_query_header TYPE STANDARD TABLE OF ts_query_header WITH EMPTY KEY .
  types:
    BEGIN OF ts_query_values,
             id TYPE zlock_t_0001-id.
        INCLUDE TYPE ts_lock_values.
    TYPES:
           END OF ts_query_values .
  types:
    tt_query_values TYPE STANDARD TABLE OF ts_query_values WITH EMPTY KEY .

    "! <p class="shorttext synchronized">Query</p>
    "! @parameter it_r_id | <p class="shorttext synchronized">Id lock ranges</p>
    "! @parameter it_r_appl | <p class="shorttext synchronized">APPL ranges</p>
    "! @parameter it_r_subobject | <p class="shorttext synchronized">Subobject ranges</p>
    "! @parameter it_r_erdat | <p class="shorttext synchronized">Creation date ranges</p>
    "! @parameter it_r_ernam | <p class="shorttext synchronized">Creation user ranges</p>
    "! @parameter it_r_erzet | <p class="shorttext synchronized">Creation time ranges</p>
    "! @parameter et_header_data | <p class="shorttext synchronized">Header data</p>
    "! @parameter et_values_data | <p class="shorttext synchronized">Values data</p>
  methods QUERY
    importing
      !IT_R_ID type TT_R_ID_LOCK optional
      !IT_R_APPL type TT_R_APPL optional
      !IT_R_SUBOBJECT type TT_R_SUBOBJECT optional
      !IT_R_ERDAT type TT_R_ERDAT optional
      !IT_R_ERNAM type TT_R_ERNAM optional
      !IT_R_ERZET type TT_R_ERZET optional
    exporting
      !ET_HEADER_DATA type TT_QUERY_HEADER
      !ET_VALUES_DATA type TT_QUERY_VALUES .
    "! <p class="shorttext synchronized">Enqueue</p>
    "! @parameter iv_appl | <p class="shorttext synchronized">Application</p>
    "! @parameter iv_subobject | <p class="shorttext synchronized">Subobject</p>
    "! @parameter it_lock_values | <p class="shorttext synchronized">Lock values</p>
    "! @parameter iv_commit | <p class="shorttext synchronized">Do commit</p>
    "! @parameter rv_id_lock | <p class="shorttext synchronized">ID Lock</p>
  methods ENQUEUE
    importing
      !IV_APPL type ZLOCK_E_APPL
      !IV_SUBOBJECT type ZLOCK_E_SUBOBJECT optional
      !IT_LOCK_VALUES type TT_LOCK_VALUES
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
    returning
      value(RV_ID_LOCK) type ZLOCK_E_ID_LOCK
    raising
      ZCX_LOCK .
    "! <p class="shorttext synchronized">Dequeue by ID</p>
    "! @parameter iv_id_lock | <p class="shorttext synchronized">ID Lock</p>
    "! @parameter iv_commit | <p class="shorttext synchronized">Do commit</p>
  methods DEQUEUE_BY_ID
    importing
      !IV_ID type ZLOCK_E_ID_LOCK
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
    raising
      ZCX_LOCK .
    "! <p class="shorttext synchronized">Dequeue</p>
    "! @parameter iv_appl | <p class="shorttext synchronized">Application</p>
    "! @parameter iv_subobject | <p class="shorttext synchronized">Subobject</p>
    "! @parameter it_lock_values | <p class="shorttext synchronized">Lock values</p>
    "! @parameter iv_commit | <p class="shorttext synchronized">Do commit</p>
  methods DEQUEUE
    importing
      !IV_APPL type ZLOCK_E_APPL
      !IV_SUBOBJECT type ZLOCK_E_SUBOBJECT
      !IT_LOCK_VALUES type TT_LOCK_VALUES
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
    raising
      ZCX_LOCK .
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_search_values_locked,
             id    TYPE zlock_t_0001-id,
             ernam TYPE zlock_t_0001-ernam,
             field TYPE zlock_t_0002-field,
             value TYPE zlock_t_0002-value,
           END OF ts_search_values_locked.
    TYPES: tt_search_values_locked TYPE HASHED TABLE OF ts_search_values_locked WITH UNIQUE KEY id field value.
    TYPES ts_lock_header TYPE zlock_t_0001.
    TYPES tt_lock_values_db TYPE STANDARD TABLE OF zlock_t_0002 WITH EMPTY KEY.

    "! <p class="shorttext synchronized">Check if the values is already locked</p>
    "! @parameter iv_appl | <p class="shorttext synchronized">Application</p>
    "! @parameter iv_subobject | <p class="shorttext synchronized">Subobject</p>
    "! @parameter et_lock_values | <p class="shorttext synchronized">Lock values</p>
    "! @parameter iv_check_all_fields | <p class="shorttext synchronized">Check that all values match the DB</p>
    "! This parameter that validates all fields only makes sense to inform in the unlocking process.
    "! The lock should not be reported because if all the fields reported already exist in the database you must skip that there is a match.
    "! @parameter ev_id_lock | <p class="shorttext synchronized">ID Lock</p>
    "! @parameter ev_user_lock | <p class="shorttext synchronized">Lock by user</p>
    METHODS search_values_already_locked
      IMPORTING
        !iv_appl             TYPE zlock_e_appl
        !iv_subobject        TYPE zlock_e_subobject OPTIONAL
        !it_lock_values      TYPE zcl_lock_management=>tt_lock_values
        !iv_check_all_fields TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !ev_id_lock          TYPE zlock_e_id_lock
        !ev_user_lock        TYPE ernam.
    "! <p class="shorttext synchronized">Generate ID Lock</p>
    "! @parameter rv_id_lock | <p class="shorttext synchronized">ID Lock</p>
    METHODS generate_id_lock
      RETURNING VALUE(rv_id_lock) TYPE zlock_e_id_lock.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_LOCK_MANAGEMENT IMPLEMENTATION.


  METHOD dequeue.
    " Se mira primero si hay un bloqueo para los datos introducidos.
    search_values_already_locked( EXPORTING iv_appl = iv_appl
                                           iv_subobject = iv_subobject
                                           it_lock_values = it_lock_values
                                           iv_check_all_fields = abap_true
                                 IMPORTING ev_id_lock = DATA(lv_id_lock) ).

    IF lv_id_lock IS NOT INITIAL. " Si hay bloqueo se elimina
      dequeue_by_id( EXPORTING iv_id     = lv_id_lock
                               iv_commit = iv_commit ).
    ENDIF.

  ENDMETHOD.


  METHOD dequeue_by_id.

    DELETE FROM zlock_t_0001 WHERE id = iv_id.
    IF sy-subrc = 0.
      DELETE FROM zlock_t_0002 WHERE id = iv_id.
    ELSE.
      RAISE EXCEPTION TYPE zcx_lock
        EXPORTING
          textid  = zcx_lock=>id_lock_not_exist
          mv_msg1 = CONV #( iv_id ).
    ENDIF.

  ENDMETHOD.


  METHOD enqueue.
    CLEAR: rv_id_lock.

    " Se mira primero si hay un bloqueo para los datos introducidos.
    search_values_already_locked( EXPORTING iv_appl = iv_appl
                                           iv_subobject = iv_subobject
                                           it_lock_values = it_lock_values
                                 IMPORTING ev_id_lock = DATA(lv_id_lock)
                                           ev_user_lock = DATA(lv_user_lock) ).

    IF lv_id_lock IS INITIAL. " No hay desbloqueo previo


      " Se informa la cabecera
      DATA(ls_header) = VALUE ts_lock_header( id = generate_id_lock(  )
                                             appl = iv_appl
                                             subobject = iv_subobject
                                             erdat = sy-datum
                                             erzet = sy-uzeit
                                             ernam = sy-uname ).

      " Se informa la posición
      DATA(lt_values) = VALUE tt_lock_values_db( FOR <wa> IN it_lock_values
                                                 ( id = ls_header-id
                                                   field = <wa>-field
                                                   value = <wa>-value ) ).

      MODIFY zlock_t_0001 FROM ls_header.
      IF sy-subrc = 0.
        MODIFY zlock_t_0002 FROM TABLE lt_values.

        IF sy-subrc = 0.
          rv_id_lock = ls_header-id.
          IF iv_commit = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.

      ELSE.

        RAISE EXCEPTION TYPE zcx_lock
          EXPORTING
            textid = zcx_lock=>error_generate_lock.

      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_lock
        EXPORTING
          textid  = zcx_lock=>already_blocked
          mv_msg1 = CONV #( lv_user_lock ).
    ENDIF.
  ENDMETHOD.


  METHOD generate_id_lock.
    rv_id_lock = /bobf/cl_frw_factory=>get_new_key( ).
  ENDMETHOD.


  METHOD query.

    CLEAR: et_header_data, et_values_data.

    SELECT id appl subobject erdat erzet ernam INTO TABLE et_header_data
           FROM zlock_t_0001
           WHERE id IN it_r_id
                 AND appl IN it_r_appl
                 AND subobject IN it_r_subobject
                 AND erdat IN it_r_erdat
                 AND erzet IN it_r_erzet
                 AND ernam IN it_r_ernam.

    IF sy-subrc = 0.
      " Creo un ranges con el ID encontrados para buscar sus valores
      DATA(lt_r_id) = VALUE tt_r_id_lock( FOR <wa> IN et_header_data ( sign = 'I' option = 'EQ' low = <wa>-id ) ).

      SELECT id field value INTO TABLE et_values_data
             FROM zlock_t_0002
             WHERE id IN lt_r_id.
    ENDIF.

  ENDMETHOD.


  METHOD search_values_already_locked.

    DATA lt_values_db TYPE tt_search_values_locked.
    DATA lt_r_subobject TYPE tt_r_subobject.

    CLEAR: ev_id_lock, ev_user_lock.

    IF iv_subobject IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_subobject ) INTO TABLE lt_r_subobject.
    ENDIF.

    " Número de lineas que tiene la tabla de valores pasada
    DESCRIBE TABLE it_lock_values LINES DATA(lv_counter_lock_values).

    " Primero se busca los bloqueos para la aplicación y subobjeto
    SELECT a~id, a~ernam, b~field, b~value INTO TABLE @lt_values_db
           FROM zlock_t_0001 AS a INNER JOIN zlock_t_0002 AS b ON
               b~id = a~id
            WHERE a~appl = @iv_appl
                  AND a~subobject IN @lt_r_subobject.
    IF sy-subrc = 0.
      " Ahora se buscan si los campos y valores
      LOOP AT lt_values_db ASSIGNING FIELD-SYMBOL(<ls_values_dummy>)
                        GROUP BY <ls_values_dummy>-id.

        DATA(lv_exist) = abap_true. " Por defecto los valores van a coincidir

        " Por cada valor pasado se busca si existe en la base de datos con el ID de bloqueo que se esta leyendo
        LOOP AT it_lock_values ASSIGNING FIELD-SYMBOL(<ls_lock_values>).
          READ TABLE lt_values_db TRANSPORTING NO FIELDS
                                    WITH TABLE KEY field = <ls_lock_values>-field
                                             value = <ls_lock_values>-value
                                             id = <ls_values_dummy>-id.
          IF sy-subrc NE 0. " Si no existe se sale del proceso.
            lv_exist = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.

        " Si el parámetro de validar todos los campos esta a TRUE y se ha encontrado una coincidencia hay que mirar que los campos que vienen por parámetros
        IF iv_check_all_fields = abap_true AND lv_exist = abap_true.
          " Se cuenta cuando registros hay con el ID lock encontrado
          DATA(lv_counter_id_lock) = REDUCE #( INIT x = 0 FOR <wa> IN lt_values_db WHERE ( id = <ls_values_dummy>-id ) NEXT x = x + 1 ).

          " Se cambia la variable que indica si existe en base al número de campos
          lv_exist = COND #( WHEN lv_counter_id_lock NE lv_counter_lock_values THEN abap_false ELSE lv_exist  ).

        ENDIF.


        IF lv_exist = abap_true.

          ev_id_lock = <ls_values_dummy>-id.
          ev_user_lock = <ls_values_dummy>-ernam.
          EXIT.

        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
