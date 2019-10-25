*"* use this source file for your ABAP unit test classes
DEFINE add_xml.
  CONCATENATE lv_xml &1 INTO lv_xml.
END-OF-DEFINITION. "#EC NEEDED

CLASS ltcl_simple_transformation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      pos_st_i_ext_req_seq_ok     FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_seq_not_ok FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_new_field  FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_field_miss FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation IMPLEMENTATION.

  METHOD pos_st_i_ext_req_seq_ok.
    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_info_ext=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository_ext>'.
    add_xml '  <url>a</url>'.
    add_xml '  <user>b</user>'.
    add_xml '  <password>c</password>'.
    add_xml '</repository_ext>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_info_ext=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url      exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user     exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password exp = 'c' ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_seq_not_ok.
    "Test: Sequence of fields in input XML is NOT equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_info_ext=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository_ext>'.
    add_xml '  <url>a</url>'.
    add_xml '  <password>c</password>'. "exchanged by USER
    add_xml '  <user>b</user>'. "exchanged by PASSWORD
    add_xml '</repository_ext>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_info_ext=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url      exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user     exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password exp = 'c' ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_new_field.
    "Test: Input XML contains a field that is not part of ABAP structure
    "Result: Transformation should succeed, new field should be ignored

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_info_ext=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository_ext>'.
    add_xml '  <url>a</url>'.
    add_xml '  <dontexist>x</dontexist>'. "field don't exist in structure
    add_xml '  <user>b</user>'.
    add_xml '  <password>c</password>'.
    add_xml '</repository_ext>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_info_ext=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url      exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user     exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password exp = 'c' ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_field_miss.
    "Test: In input XML a field defined in ABAP structure is missing
    "Result: Transformation should succeed, missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_info_ext=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository_ext>'.
    add_xml '  <url>a</url>'.
    "add_xml '  <user>b</user>'.  "field missing(!)
    add_xml '  <password>c</password>'.
    add_xml '</repository_ext>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_info_ext=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url      exp = 'a' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-user ). "initial value expected
    cl_abap_unit_assert=>assert_equals( act = ls_data-password exp = 'c' ).

  ENDMETHOD.

ENDCLASS.
