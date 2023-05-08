class ZCL_IDOC_EDIDD_SEGMENT definition
  public
  inheriting from ZCL_IDOC_EDIDD
  final
  create public

  global friends ZCL_IDOC_EDIDD .

public section.

  methods CONSTRUCTOR
    importing
      !IV_IDOC_TYPE type EDI_IDOCTP
      !IV_IDOC_EXTENSION type EDI_CIMTYP
      !IS_EDIDD type EDIDD
      !IT_IDOC_STRUCTURE type FKK_EDI_IAPI06_TT
    raising
      ZCX_IDOC_EXCEPTIONS .
  methods GET_HIERARCHY_LEVEL
    returning
      value(RV_VALUE) type EDI_HLEVEL .
  methods GET_IDOC_NUMBER
    returning
      value(RV_VALUE) type EDI_DOCNUM .
  methods GET_NAME
    returning
      value(RV_VALUE) type EDILSEGTYP .
  methods GET_PARENT_NUMBER
    returning
      value(RV_VALUE) type EDI_PSGNUM .
  methods GET_SDATA
    returning
      value(RS_VALUE) type EDI_SDATA .
  methods GET_SEGMENT_NUMBER
    returning
      value(RV_VALUE) type IDOCDSGNUM .
  methods SET_SDATA
    importing
      !IS_VALUE type ANY
    returning
      value(RO_INSTANCE) type ref to ZCL_IDOC_EDIDD_SEGMENT .

  methods ADD_SEGMENT
    redefinition .
  methods GET_EDIDD
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA as_edidd TYPE edidd .
ENDCLASS.



CLASS ZCL_IDOC_EDIDD_SEGMENT IMPLEMENTATION.


  METHOD add_segment.
    READ TABLE at_idoc_structure_sorted REFERENCE INTO DATA(ld_idoc_structure)
      WITH KEY segtyp COMPONENTS segtyp = is_edidd-segnam.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_idoc_exceptions
        EXPORTING
          textid = zcx_idoc_exceptions=>segment_incorrect_for_idoc
          msgv1  = is_edidd-segnam
          msgv2  = av_idoc_type
          msgv3  = av_idoc_extension.
    ENDIF.

    IF get_name( ) <> ld_idoc_structure->parseg.
      RAISE EXCEPTION TYPE zcx_idoc_exceptions
        EXPORTING
          textid = zcx_idoc_exceptions=>segment_child_of_segment
          msgv1  = is_edidd-segnam
          msgv2  = ld_idoc_structure->parseg.
    ENDIF.

    ro_segment = add_segment_do( is_edidd ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
      iv_idoc_type = iv_idoc_type
      iv_idoc_extension = iv_idoc_extension
      it_idoc_structure = it_idoc_structure ).

    as_edidd = is_edidd.
  ENDMETHOD.


  METHOD get_edidd.
    DATA:
      lo_segment TYPE REF TO zcl_idoc_edidd_segment.

    APPEND as_edidd TO rt_edidd.

    DATA(lo_iterator) = ao_segments->get_iterator( ).
    WHILE lo_iterator->has_next( ).
      lo_segment ?= lo_iterator->get_next( ).
      DATA(lt_edidd) = lo_segment->get_edidd( ).
      APPEND LINES OF lt_edidd TO rt_edidd.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_name.
    rv_value = as_edidd-segnam.
  ENDMETHOD.


  METHOD get_sdata.
    rs_value = as_edidd-sdata.
  ENDMETHOD.


  METHOD set_sdata.
    MOVE is_value TO as_edidd-sdata.
    ro_instance = me.
  ENDMETHOD.


  METHOD get_hierarchy_level.
    rv_value = as_edidd-hlevel.
  ENDMETHOD.


  METHOD get_idoc_number.
    rv_value = as_edidd-docnum.
  ENDMETHOD.


  METHOD get_parent_number.
    rv_value = as_edidd-psgnum.
  ENDMETHOD.


  METHOD get_segment_number.
    rv_value = as_edidd-segnum.
  ENDMETHOD.
ENDCLASS.