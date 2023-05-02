CLASS zcl_idoc_edidd_segment DEFINITION
  PUBLIC
  INHERITING FROM zcl_idoc_edidd
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_idoc_edidd .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_idoc_type      TYPE edi_idoctp
        !iv_idoc_extension TYPE edi_cimtyp
        !is_edidd          TYPE edidd
        !it_idoc_structure TYPE fkk_edi_iapi06_tt
      RAISING
        zcx_idoc_exceptions .
    METHODS get_name
      RETURNING
        VALUE(rv_name) TYPE edilsegtyp .
    METHODS get_sdata
      RETURNING
        VALUE(rs_sdata) TYPE edi_sdata .
    METHODS get_segment_number
      RETURNING
        VALUE(rv_number) TYPE idocdsgnum
    METHODS set_sdata
      IMPORTING
        !is_sdata TYPE any .

    METHODS add_segment
         REDEFINITION .
    METHODS get_edidd
         REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA as_edidd TYPE edidd .
ENDCLASS.



CLASS zcl_idoc_edidd_segment IMPLEMENTATION.


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
    rv_name = as_edidd-segnam.
  ENDMETHOD.


  METHOD get_sdata.
    rs_sdata = as_edidd-sdata.
  ENDMETHOD.


  METHOD get_segment_number.
    rv_number = as_edidd-segnum.
  ENDMETHOD.


  METHOD set_sdata.
    MOVE is_sdata TO as_edidd-sdata.
  ENDMETHOD.
ENDCLASS.
