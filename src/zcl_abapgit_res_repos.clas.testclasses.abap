*"* use this source file for your ABAP unit test classes

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
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository>|.
    lv_xml = lv_xml && |  <branch>a</branch>|.
    lv_xml = lv_xml && |  <package>b</package>|.
    lv_xml = lv_xml && |  <transportRequest>c</transportRequest>|.
    lv_xml = lv_xml && |  <url>d</url>|.
    lv_xml = lv_xml && |  <user>e</user>|.
    lv_xml = lv_xml && |  <password>f</password>|.
    lv_xml = lv_xml && |</repository>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT (lt_result).

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
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository>|.
    lv_xml = lv_xml && |  <branch>a</branch>|.
    lv_xml = lv_xml && |  <package>b</package>|.
    lv_xml = lv_xml && |  <transportRequest>c</transportRequest>|.
    lv_xml = lv_xml && |  <user>e</user>|. "exchanged by URL
    lv_xml = lv_xml && |  <url>d</url>|. "exchanged by USER
    lv_xml = lv_xml && |  <password>f</password>|.
    lv_xml = lv_xml && |</repository>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT (lt_result).

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
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">|.
    lv_xml = lv_xml && |  <abapgitrepo:branchName>a</abapgitrepo:branchName>|.
    lv_xml = lv_xml && |  <abapgitrepo:package>b</abapgitrepo:package>|.
    lv_xml = lv_xml && |  <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>|.
    lv_xml = lv_xml && |  <abapgitrepo:newField>x</abapgitrepo:newField>|.
    lv_xml = lv_xml && |  <abapgitrepo:url>d</abapgitrepo:url>|.
    lv_xml = lv_xml && |  <abapgitrepo:remoteUser>e</abapgitrepo:remoteUser>|.
    lv_xml = lv_xml && |  <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>|.
    lv_xml = lv_xml && |</abapgitrepo:repository>|.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT (lt_result).

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
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">|.
    lv_xml = lv_xml && |  <abapgitrepo:branchName>a</abapgitrepo:branchName>|.
    lv_xml = lv_xml && |  <abapgitrepo:package>b</abapgitrepo:package>|.
    lv_xml = lv_xml && |  <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>|.
   "lv_xml = lv_xml && |  <abapgitrepo:url>d</abapgitrepo:url>|.  url is missing
    lv_xml = lv_xml && |  <abapgitrepo:remoteUser>e</abapgitrepo:remoteUser>|.
    lv_xml = lv_xml && |  <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>|.
    lv_xml = lv_xml && |</abapgitrepo:repository>|.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION zabapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-url ). "initial value expected
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).

  ENDMETHOD.

  METHOD repo_v2_deserialize_ok.
    DATA: lv_xml_data        TYPE string,
          lv_repository_data TYPE zcl_abapgit_res_repos=>tt_request_data.

    lv_xml_data = lv_xml_data && |<?xml version="1.0" encoding="UTF-8"?><repositories>|.
    lv_xml_data = lv_xml_data && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |  <repository>| && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |    <branch>refs/heads/master</branch>| && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |    <package>TEST_YY</package>| && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |    <url>https://github.com/Wunderfitz/yy.git</url>|.
    lv_xml_data = lv_xml_data && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |  </repository>| && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |  <repository>| && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |    <branch>refs/heads/master</branch>| && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |    <package>TEST_LOGGER</package>| && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |    <url>https://github.com/epeterson320/ABAP-Logger.git</url>|.
    lv_xml_data = lv_xml_data && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |  </repository>| && cl_abap_char_utilities=>newline.
    lv_xml_data = lv_xml_data && |</repositories>|.

    CALL TRANSFORMATION zabapgit_st_repo_post_v2
      SOURCE XML lv_xml_data
      RESULT repositories = lv_repository_data.

    cl_abap_unit_assert=>assert_not_initial( lv_repository_data ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lv_repository_data ) ).
  ENDMETHOD.

  METHOD repo_v2_serialize_ok.
    DATA: lv_repository_data TYPE zcl_abapgit_res_repos=>tt_request_data,
          lv_xml_data        TYPE string.

    lv_repository_data = VALUE #( (
                                  url = 'https://github.com/Wunderfitz/yy.git'
                                  branch = 'refs/heads/master'
                                  package = 'TEST_YY'
                                )
                                (
                                  url = 'https://github.com/epeterson320/ABAP-Logger.git'
                                  branch = 'refs/heads/master'
                                  package = 'TEST_LOGGER' ) ).

    CALL TRANSFORMATION zabapgit_st_repo_post_v2
      SOURCE repositories = lv_repository_data
      RESULT XML lv_xml_data.

    cl_abap_unit_assert=>assert_not_initial( lv_xml_data ).
    cl_abap_unit_assert=>assert_true( boolc( lv_xml_data CS '<package>TEST_YY</package>' ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_provider_default DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_abapgit_provider.
ENDCLASS.

CLASS lcl_abapgit_provider_default IMPLEMENTATION.

  METHOD lif_abapgit_provider~list_repositories.

  ENDMETHOD.

  METHOD lif_abapgit_provider~perform_import.
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

  METHOD lif_abapgit_provider~set_authentication_info.

  ENDMETHOD.

  METHOD lif_abapgit_provider~validate_package.
    CASE iv_package.
      WHEN 'TESCHD_JAK'.
        " Good
      WHEN 'TESCHD_YY'.
        " Good
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( msg = |Unexpected package { iv_package } | ).
    ENDCASE.
  ENDMETHOD.

  METHOD lif_abapgit_provider~validate_transport_request.

    cl_abap_unit_assert=>assert_initial( iv_transport_request ).

  ENDMETHOD.

  METHOD lif_abapgit_provider~is_tr_check_required.
    CLEAR: rv_is_required.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_abapgit_repos_resource DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: mv_request_stub           TYPE REF TO cl_adt_rest_request_stub,
          mv_response_spy           TYPE REF TO cl_adt_rest_response_spy,
          mv_abapgit_repos_resource TYPE REF TO zcl_abapgit_res_repos,
          mv_request_data_v2        TYPE zcl_abapgit_res_repos=>tt_request_data.
    METHODS:
      setup,
      no_content_type_header FOR TESTING RAISING cx_static_check,
      standard_v2 FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_abapgit_repos_resource IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->mv_request_stub.
    CREATE OBJECT me->mv_response_spy.
    CREATE OBJECT me->mv_abapgit_repos_resource.
    me->mv_abapgit_repos_resource->set_abapgit_provider( io_abapgit_provider = NEW lcl_abapgit_provider_default( ) ).
  ENDMETHOD.

  METHOD no_content_type_header.
    me->mv_abapgit_repos_resource->post( request  = me->mv_request_stub
                                      response = me->mv_response_spy ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_client_error_bad_request
                                        act = me->mv_response_spy->get_status( ) ).
  ENDMETHOD.

  METHOD standard_v2.

    me->mv_request_stub->add_header_field( key   = if_http_header_fields=>content_type
                                        value = zcl_abapgit_res_repos=>co_content_type_repo_v2 ).

    me->mv_request_data_v2 = VALUE #( (
                                      url = 'https://github.com/Wunderfitz/jak.git'
                                      branch = 'refs/heads/master'
                                      package = 'TESCHD_JAK'
                                    )
                                    (
                                      url = 'https://github.com/Wunderfitz/yy.git'
                                      branch = 'refs/heads/master'
                                      package = 'TESCHD_YY'
                                    ) ).

    me->mv_request_stub->set_body_data( data = me->mv_request_data_v2 ).

    me->mv_abapgit_repos_resource->post( request  = me->mv_request_stub
                                      response = me->mv_response_spy ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_created
                                        act = me->mv_response_spy->get_status( ) ).

  ENDMETHOD.
ENDCLASS.
