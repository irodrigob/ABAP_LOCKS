class ZCX_LOCK definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ALREADY_BLOCKED,
      msgid type symsgid value 'ZLOCK',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MV_MSG1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ALREADY_BLOCKED .
  constants:
    begin of ERROR_GENERATE_LOCK,
      msgid type symsgid value 'ZLOCK',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_GENERATE_LOCK .
  constants:
    begin of ID_LOCK_NOT_EXIST,
      msgid type symsgid value 'ZLOCK',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MV_MSG1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ID_LOCK_NOT_EXIST .
  data MV_MSG1 type STRING .
  data MV_MSG2 type STRING .
  data MV_MSG3 type STRING .
  data MV_MSG4 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MSG1 type STRING optional
      !MV_MSG2 type STRING optional
      !MV_MSG3 type STRING optional
      !MV_MSG4 type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_LOCK IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_MSG1 = MV_MSG1 .
me->MV_MSG2 = MV_MSG2 .
me->MV_MSG3 = MV_MSG3 .
me->MV_MSG4 = MV_MSG4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
