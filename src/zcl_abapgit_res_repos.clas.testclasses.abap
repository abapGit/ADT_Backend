*"* use this source file for your ABAP unit test classes

*CLASS ltcl_simple_transformation DEFINITION DEFERRED.
*CLASS zcl_abapgit_res_repos DEFINITION LOCAL FRIENDS ltcl_simple_transformation.

CLASS ltcl_simple_transformation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      pos_st_repo_post_seq_ok     FOR TESTING RAISING cx_static_check,
      pos_st_repo_post_seq_not_ok FOR TESTING RAISING cx_static_check,
      pos_st_repo_post_new_field  FOR TESTING RAISING cx_static_check,
      pos_st_repo_post_field_miss FOR TESTING RAISING cx_static_check,
      repo_v2_deserialize_ok      FOR TESTING RAISING cx_static_check,
      repo_v2_serialize_ok        FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation IMPLEMENTATION.

  METHOD pos_st_repo_post_seq_ok.
    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <package>b</package>'.
    add_xml '  <transportRequest>c</transportRequest>'.
    add_xml '  <url>d</url>'.
    add_xml '  <user>e</user>'.
    add_xml '  <password>f</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).

  ENDMETHOD.

  METHOD pos_st_repo_post_seq_not_ok.
    "Test: Sequence of fields in input XML is NOT equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <package>b</package>'.
    add_xml '  <transportRequest>c</transportRequest>'.
    add_xml '  <user>e</user>'. "exchanged by URL
    add_xml '  <url>d</url>'. "exchanged by USER
    add_xml '  <password>f</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).

  ENDMETHOD.

  METHOD pos_st_repo_post_new_field.
    "Test: Input XML contains a field that is not part of ABAP structure
    "Result: Transformation should succeed, new field should be ignored

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <package>b</package>'.
    add_xml '  <transportRequest>c</transportRequest>'.
    add_xml '  <dontexist>x</dontexist>'. "field don't exist in structure
    add_xml '  <url>d</url>'.
    add_xml '  <user>e</user>'.
    add_xml '  <password>f</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).

  ENDMETHOD.

  METHOD pos_st_repo_post_field_miss.
    "Test: In input XML a field defined in ABAP structure is missing
    "Result: Transformation should succeed, missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <package>b</package>'.
    add_xml '  <transportRequest>c</transportRequest>'.
    "add_xml '  <url>d</url>'. "field missing(!)
    add_xml '  <user>e</user>'.
    add_xml '  <password>f</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-url ). "initial value expected
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).

  ENDMETHOD.

  METHOD repo_v2_deserialize_ok.
    DATA: xml_data        TYPE string,
          repository_data TYPE zcl_abapgit_res_repos=>tt_request_data.

    xml_data = xml_data && |<?xml version="1.0" encoding="UTF-8"?><repositories>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |  <repository>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <branch>refs/heads/master</branch>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <package>TEST_YY</package>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <url>https://github.com/Wunderfitz/yy.git</url>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |  </repository>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |  <repository>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <branch>refs/heads/master</branch>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <package>TEST_LOGGER</package>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <url>https://github.com/epeterson320/ABAP-Logger.git</url>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |  </repository>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |</repositories>|.

    CALL TRANSFORMATION abapgit_st_repo_post_v2
      SOURCE XML xml_data
      RESULT repositories = repository_data.

    cl_abap_unit_assert=>assert_not_initial( repository_data ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( repository_data ) ).
  ENDMETHOD.

  METHOD repo_v2_serialize_ok.
    DATA: repository_data TYPE zcl_abapgit_res_repos=>tt_request_data,
          xml_data        TYPE string.

    repository_data = VALUE #( ( url = 'https://github.com/Wunderfitz/yy.git' branch = 'refs/heads/master' package = 'TEST_YY' )
                               ( url = 'https://github.com/epeterson320/ABAP-Logger.git' branch = 'refs/heads/master' package = 'TEST_LOGGER' ) ).

    CALL TRANSFORMATION abapgit_st_repo_post_v2
      SOURCE repositories = repository_data
      RESULT XML xml_data.

    cl_abap_unit_assert=>assert_not_initial( xml_data ).
    cl_abap_unit_assert=>assert_true( boolc( xml_data CS '<package>TEST_YY</package>' ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltd_abapgit_provider_default DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lzif_abapgit_provider.
ENDCLASS.

CLASS ltd_abapgit_provider_default IMPLEMENTATION.

  METHOD lzif_abapgit_provider~list_repositories.

  ENDMETHOD.

  METHOD lzif_abapgit_provider~perform_import.
    CASE is_request_data-url.
      WHEN 'https://github.com/Wunderfitz/jak.git'.
        cl_abap_unit_assert=>assert_equals( exp = 'refs/heads/master' act = is_request_data-branch ).
        cl_abap_unit_assert=>assert_equals( exp = 'TESCHD_JAK' act = is_request_data-package ).
      WHEN 'https://github.com/Wunderfitz/yy.git'.
        cl_abap_unit_assert=>assert_equals( exp = 'refs/heads/master' act = is_request_data-branch ).
        cl_abap_unit_assert=>assert_equals( exp = 'TESCHD_YY' act = is_request_data-package ).
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( msg = |Unexpected Git repository { is_request_data-url } | ).
    ENDCASE.
  ENDMETHOD.

  METHOD lzif_abapgit_provider~set_authentication_info.

  ENDMETHOD.

  METHOD lzif_abapgit_provider~validate_package.
    CASE iv_package.
      WHEN 'TESCHD_JAK'.
        " Good
      WHEN 'TESCHD_YY'.
        " Good
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( msg = |Unexpected package { iv_package } | ).
    ENDCASE.
  ENDMETHOD.

  METHOD lzif_abapgit_provider~validate_transport_request.

    cl_abap_unit_assert=>assert_initial( iv_transport_request ).

  ENDMETHOD.

  METHOD lzif_abapgit_provider~is_tr_check_required.
    CLEAR: rv_is_required.
  ENDMETHOD.

ENDCLASS.

CLASS ltzcl_abapgit_repos_resource DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: request_stub           TYPE REF TO cl_adt_rest_request_stub,
          response_spy           TYPE REF TO cl_adt_rest_response_spy,
          abapgit_repos_resource TYPE REF TO zcl_abapgit_res_repos,
          request_data_v2        TYPE zcl_abapgit_res_repos=>tt_request_data.
    METHODS:
      setup,
      no_content_type_header FOR TESTING RAISING cx_static_check,
      standard_v2 FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltzcl_abapgit_repos_resource IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->request_stub.
    CREATE OBJECT me->response_spy.
    CREATE OBJECT me->abapgit_repos_resource.
    me->abapgit_repos_resource->set_abapgit_provider( io_abapgit_provider = NEW ltd_abapgit_provider_default( ) ).
  ENDMETHOD.

  METHOD no_content_type_header.
    me->abapgit_repos_resource->post( EXPORTING request  = me->request_stub
                                                response = me->response_spy ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_client_error_bad_request act = me->response_spy->get_status( ) ).
  ENDMETHOD.

  METHOD standard_v2.

    me->request_stub->add_header_field( EXPORTING key   = if_http_header_fields=>content_type
                                                  value = zcl_abapgit_res_repos=>co_content_type_repo_v2 ).

    me->request_data_v2 = VALUE #( ( url = 'https://github.com/Wunderfitz/jak.git' branch = 'refs/heads/master' package = 'TESCHD_JAK' )
                                   ( url = 'https://github.com/Wunderfitz/yy.git' branch = 'refs/heads/master' package = 'TESCHD_YY' ) ).

    me->request_stub->set_body_data( data = me->request_data_v2 ).

    me->abapgit_repos_resource->post( EXPORTING request  = me->request_stub
                                                response = me->response_spy ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_created act = me->response_spy->get_status( ) ).

  ENDMETHOD.

ENDCLASS.
