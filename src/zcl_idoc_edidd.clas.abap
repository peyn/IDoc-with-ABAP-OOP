CLASS zcl_idoc_edidd DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    CLASS-METHODS create_with_data
      IMPORTING
        !iv_idoc_type      TYPE edi_idoctp
        !iv_idoc_extension TYPE edi_cimtyp
        !it_edidd          TYPE edidd_tt OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_idoc_edidd
      RAISING
        zcx_idoc_exceptions .
    METHODS add_segment
      IMPORTING
        !is_edidd         TYPE edidd
      RETURNING
        VALUE(ro_segment) TYPE REF TO zcl_idoc_edidd_segment
      RAISING
        zcx_idoc_exceptions .
    METHODS get_edidd
      RETURNING
        VALUE(rt_edidd) TYPE edidd_tt .
    METHODS get_segments
      IMPORTING
        !iv_name           TYPE edilsegtyp OPTIONAL
      RETURNING
        VALUE(ro_segments) TYPE REF TO cl_object_collection .
    METHODS remove_segment
      IMPORTING
        !io_segment       TYPE REF TO zcl_idoc_edidd_segment
      RETURNING
        VALUE(rv_removed) TYPE flag .
  PROTECTED SECTION.

    TYPES:
      ygt_idoc_structure_sorted TYPE SORTED TABLE OF edi_iapi06
        WITH UNIQUE KEY idoctyp cimtyp nr
        WITH UNIQUE SORTED KEY segtyp COMPONENTS segtyp .

    DATA ao_segments TYPE REF TO cl_object_collection .
    DATA at_idoc_structure TYPE fkk_edi_iapi06_tt .
    DATA at_idoc_structure_sorted TYPE ygt_idoc_structure_sorted .
    DATA av_idoc_extension TYPE edi_cimtyp .
    DATA av_idoc_type TYPE edi_idoctp .

    METHODS add_segment_do
          FINAL
      IMPORTING
        !is_edidd         TYPE edidd
      RETURNING
        VALUE(ro_segment) TYPE REF TO zcl_idoc_edidd_segment
      RAISING
        zcx_idoc_exceptions .
    METHODS constructor
      IMPORTING
        !iv_idoc_type      TYPE edi_idoctp
        !iv_idoc_extension TYPE edi_cimtyp
        !it_edidd          TYPE edidd_tt OPTIONAL
        !it_idoc_structure TYPE fkk_edi_iapi06_tt
      RAISING
        zcx_idoc_exceptions .
  PRIVATE SECTION.

    METHODS add_segments_in_given_sequence
      IMPORTING
        !it_edidd TYPE edidd_tt
      RAISING
        zcx_idoc_exceptions .
ENDCLASS.



CLASS zcl_idoc_edidd IMPLEMENTATION.


  METHOD add_segment.
    DATA:
      lo_parent TYPE REF TO zcl_idoc_edidd_segment.

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

    IF ld_idoc_structure->parseg IS INITIAL.
      ro_segment = add_segment_do( is_edidd ).
    ELSE.
      DATA(lo_segments) = get_segments( ld_idoc_structure->parseg ).
      IF lo_segments->is_empty( ).
        RAISE EXCEPTION TYPE zcx_idoc_exceptions
          EXPORTING
            textid = zcx_idoc_exceptions=>parent_segment_not_found
            msgv1  = ld_idoc_structure->parseg.
      ENDIF.

      " add new segment to the last parent segment
      DATA(lo_iterator) = lo_segments->get_iterator( ).
      WHILE lo_iterator->has_next( ).
        lo_parent ?= lo_iterator->get_next( ).
      ENDWHILE.

      ro_segment = lo_parent->add_segment( is_edidd ).
    ENDIF.
  ENDMETHOD.


  METHOD add_segments_in_given_sequence.
    TYPES:
      BEGIN OF yls_last_segment,
        number  TYPE posno,
        segment TYPE REF TO zcl_idoc_edidd_segment,
      END OF yls_last_segment,
      ylt_last_segments TYPE SORTED TABLE OF yls_last_segment
        WITH UNIQUE KEY number.

    DATA:
      lt_last_segments TYPE ylt_last_segments,
      lo_segment       TYPE REF TO zcl_idoc_edidd_segment.

    LOOP AT it_edidd REFERENCE INTO DATA(ld_edidd).
      READ TABLE at_idoc_structure_sorted REFERENCE INTO DATA(ld_idoc_structure)
        WITH KEY segtyp COMPONENTS segtyp = ld_edidd->segnam.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_idoc_exceptions
          EXPORTING
            textid = zcx_idoc_exceptions=>segment_incorrect_for_idoc
            msgv1  = ld_edidd->segnam
            msgv2  = av_idoc_type
            msgv3  = av_idoc_extension.
      ENDIF.

      DATA(lo_parent) = me.
      IF ld_idoc_structure->parpno IS NOT INITIAL.
        READ TABLE lt_last_segments REFERENCE INTO DATA(ld_last_segment)
          WITH TABLE KEY number = ld_idoc_structure->parpno.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_idoc_exceptions
            EXPORTING
              textid = zcx_idoc_exceptions=>parent_segment_not_found
              msgv1  = ld_idoc_structure->parseg.
        ENDIF.

        lo_parent = ld_last_segment->segment.
      ENDIF.

      CREATE OBJECT lo_segment
        EXPORTING
          iv_idoc_type      = av_idoc_type
          iv_idoc_extension = av_idoc_extension
          is_edidd          = ld_edidd->*
          it_idoc_structure = at_idoc_structure.
      lo_parent->ao_segments->add( lo_segment ).

      READ TABLE lt_last_segments REFERENCE INTO ld_last_segment
        WITH TABLE KEY number = ld_idoc_structure->nr.
      IF sy-subrc <> 0.
        CREATE DATA ld_last_segment.
        ld_last_segment->number = ld_idoc_structure->nr.
        INSERT ld_last_segment->* INTO TABLE lt_last_segments REFERENCE INTO ld_last_segment.
      ENDIF.

      ld_last_segment->segment = lo_segment.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_segment_do.
    DATA:
      lv_added    TYPE flag VALUE abap_false,
      lt_segments TYPE TABLE OF REF TO zcl_idoc_edidd_segment,
      lo_segment  TYPE REF TO zcl_idoc_edidd_segment.

    CREATE OBJECT ro_segment
      EXPORTING
        iv_idoc_type      = av_idoc_type
        iv_idoc_extension = av_idoc_extension
        is_edidd          = is_edidd
        it_idoc_structure = at_idoc_structure.

    DATA(lo_iterator) = ao_segments->get_iterator( ).
    WHILE lo_iterator->has_next( ).
      lo_segment ?= lo_iterator->get_next( ).
      APPEND lo_segment TO lt_segments.
    ENDWHILE.

    ao_segments->clear( ).

    READ TABLE at_idoc_structure_sorted REFERENCE INTO DATA(ld_idoc_structure)
      WITH KEY segtyp COMPONENTS segtyp = is_edidd-segnam.
    DATA(lv_new_segment_position) = ld_idoc_structure->nr.

    LOOP AT lt_segments INTO lo_segment.
      " new segments still needs to be added
      IF lv_added = abap_false.
        READ TABLE at_idoc_structure_sorted REFERENCE INTO ld_idoc_structure
          WITH KEY segtyp COMPONENTS segtyp = lo_segment->get_name( ).
        DATA(lv_old_segment_position) = ld_idoc_structure->nr.

        IF lv_old_segment_position > lv_new_segment_position.
          ao_segments->add( ro_segment ).
          lv_added = abap_true.
        ENDIF.
      ENDIF.

      ao_segments->add( lo_segment ).
    ENDLOOP.

    " if segment was not inserted between other segments
    " then add it to the end of the collection
    CHECK lv_added = abap_false.
    ao_segments->add( ro_segment ).
  ENDMETHOD.


  METHOD constructor.
    av_idoc_type = iv_idoc_type.
    av_idoc_extension = iv_idoc_extension.
    at_idoc_structure[] = it_idoc_structure[].
    at_idoc_structure_sorted[] = it_idoc_structure[].

    CREATE OBJECT ao_segments.

    CHECK it_edidd[] IS NOT INITIAL.
    add_segments_in_given_sequence( it_edidd ).
  ENDMETHOD.


  METHOD create_with_data.
    DATA:
      lt_idoc_structure TYPE fkk_edi_iapi06_tt.

    CALL FUNCTION 'EDI_IDOC_SYNTAX_GET'
      EXPORTING
        pi_idoctyp       = iv_idoc_type
        pi_cimtyp        = iv_idoc_extension
      TABLES
        pt_syntax_table  = lt_idoc_structure
      EXCEPTIONS
        syntax_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_idoc_exceptions.
    ENDIF.

    CREATE OBJECT ro_instance
      EXPORTING
        iv_idoc_type      = iv_idoc_type
        iv_idoc_extension = iv_idoc_extension
        it_edidd          = it_edidd
        it_idoc_structure = lt_idoc_structure.
  ENDMETHOD.


  METHOD get_edidd.
    DATA:
      lo_segment TYPE REF TO zcl_idoc_edidd_segment.

    DATA(lo_iterator) = ao_segments->get_iterator( ).
    WHILE lo_iterator->has_next( ).
      lo_segment ?= lo_iterator->get_next( ).
      DATA(lt_edidd) = lo_segment->get_edidd( ).
      APPEND LINES OF lt_edidd TO rt_edidd.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_segments.
    DATA:
      lv_found         TYPE flag VALUE abap_false,
      lo_segment       TYPE REF TO zcl_idoc_edidd_segment,
      lo_child_segment TYPE REF TO zcl_idoc_edidd_segment.

    CREATE OBJECT ro_segments.

    DATA(lo_iterator) = ao_segments->get_iterator( ).
    WHILE lo_iterator->has_next( ).
      lo_segment ?= lo_iterator->get_next( ).

      IF iv_name IS SUPPLIED.
        IF lo_segment->get_name( ) = iv_name.
          lv_found = abap_true.
          ro_segments->add( lo_segment ).
        ENDIF.
      ELSE.
        ro_segments->add( lo_segment ).
      ENDIF.

      " if segments with given name was found on this level
      " then do not search deeper
      CHECK lv_found = abap_false.

      DATA(lo_child_segments) = lo_segment->get_segments( iv_name = iv_name ).
      DATA(lo_child_iterator) = lo_child_segments->get_iterator( ).
      WHILE lo_child_iterator->has_next( ).
        lo_child_segment ?= lo_child_iterator->get_next( ).
        ro_segments->add( lo_child_segment ).
      ENDWHILE.
    ENDWHILE.
  ENDMETHOD.


  METHOD remove_segment.
    DATA:
      lo_segment TYPE REF TO zcl_idoc_edidd_segment.

    rv_removed = abap_false.

    DATA(lo_iterator) = ao_segments->get_iterator( ).
    WHILE lo_iterator->has_next( ).
      lo_segment ?= lo_iterator->get_next( ).
      IF lo_segment = io_segment.
        ao_segments->remove( io_segment ).
        rv_removed = abap_true.
        RETURN.
      ENDIF.

      IF lo_segment->remove_segment( io_segment ).
        rv_removed = abap_true.
        RETURN.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
