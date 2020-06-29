*"* use this source file for your ABAP unit test classes

CLASS ltcl_simple_transformation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      pos_st_repo_pull_seq_ok     FOR TESTING RAISING cx_static_check,
      pos_st_repo_pull_seq_not_ok FOR TESTING RAISING cx_static_check,
      pos_st_repo_pull_new_field  FOR TESTING RAISING cx_static_check,
      pos_st_repo_pull_field_miss FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation IMPLEMENTATION.

  METHOD pos_st_repo_pull_seq_ok.
    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_obj_log=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository>|.
    lv_xml = lv_xml && |  <branch>a</branch>|.
    lv_xml = lv_xml && |  <transportRequest>b</transportRequest>|.
    lv_xml = lv_xml && |  <user>c</user>|.
    lv_xml = lv_xml && |  <password>d</password>|.
    lv_xml = lv_xml && |</repository>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_obj_log=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_pull
      SOURCE XML lv_input_xml
      RESULT (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

  METHOD pos_st_repo_pull_seq_not_ok.
    "Test: Sequence of fields in input XML is NOT equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_obj_log=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository>|.
    lv_xml = lv_xml && |  <branch>a</branch>|.
    lv_xml = lv_xml && |  <user>c</user>|. "exchanged by TRANSPORTREQUEST
    lv_xml = lv_xml && |  <transportRequest>b</transportRequest>|. "exchanged by USER
    lv_xml = lv_xml && |  <password>d</password>|.
    lv_xml = lv_xml && |</repository>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_obj_log=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_pull
      SOURCE XML lv_input_xml
      RESULT (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

  METHOD pos_st_repo_pull_new_field.
    "Test: Input XML contains a field that is not part of ABAP structure
    "Result: Transformation should succeed, new field should be ignored

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_obj_log=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository>|.
    lv_xml = lv_xml && |  <branch>a</branch>|.
    lv_xml = lv_xml && |  <transportRequest>b</transportRequest>|.
    lv_xml = lv_xml && |  <dontexist>x</dontexist>|. "field don't exist in structure
    lv_xml = lv_xml && |  <user>c</user>|.
    lv_xml = lv_xml && |  <password>d</password>|.
    lv_xml = lv_xml && |</repository>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_obj_log=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_pull
      SOURCE XML lv_input_xml
      RESULT (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

  METHOD pos_st_repo_pull_field_miss.
    "Test: In input XML a field defined in ABAP structure is missing
    "Result: Transformation should succeed, missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_obj_log=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository>|.
    lv_xml = lv_xml && |  <branch>a</branch>|.
    " lv_xml = lv_xml && |  <transportRequest>b</transportRequest>|. "field missing(!)
    lv_xml = lv_xml && |  <user>c</user>|.
    lv_xml = lv_xml && |  <password>d</password>|.
    lv_xml = lv_xml && |</repository>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_obj_log=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_pull
      SOURCE XML lv_input_xml
      RESULT (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-transportrequest ). "initial value expected
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

ENDCLASS.
