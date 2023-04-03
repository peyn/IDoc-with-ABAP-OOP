class ZCX_IDOC_EXCEPTIONS definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    " Segment &1 is incorrect for IDoc type &2/&3
    BEGIN OF segment_incorrect_for_idoc,
        msgid TYPE symsgid VALUE 'ZCL_IDOC_MESSAGES',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF segment_incorrect_for_idoc .
  constants:
    " Parent segment &1 not found
    begin of PARENT_SEGMENT_NOT_FOUND,
      msgid type symsgid value 'ZCL_IDOC_MESSAGES',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PARENT_SEGMENT_NOT_FOUND .
  constants:
    " Segment &1 should be a child of segment &2
    begin of SEGMENT_CHILD_OF_SEGMENT,
      msgid type symsgid value 'ZCL_IDOC_MESSAGES',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEGMENT_CHILD_OF_SEGMENT .
  data MSGV1 type SYST_MSGV read-only .
  data MSGV2 type SYST_MSGV read-only .
  data MSGV3 type SYST_MSGV read-only .
  data MSGV4 type SYST_MSGV read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type CLIKE optional
      !MSGV2 type CLIKE optional
      !MSGV3 type CLIKE optional
      !MSGV4 type CLIKE optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_IDOC_EXCEPTIONS IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
