CLASS zcl_abapgit_res_repo_pull DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF  ty_s_result,
        reason  TYPE string,
        message TYPE string,
      END OF ty_s_result.
    TYPES:
      BEGIN OF ty_request_pull_data,
        branch           TYPE string,
        transportrequest TYPE string,
        user             TYPE string,
        password         TYPE string,
      END OF ty_request_pull_data.
    TYPES: BEGIN OF ty_repo_w_links.
             INCLUDE  TYPE zif_abapgit_persistence=>ty_repo.
             TYPES:   links TYPE if_atom_types=>link_t.
    TYPES: END OF ty_repo_w_links.
    TYPES:
      tt_repo_w_links TYPE STANDARD TABLE OF ty_repo_w_links WITH DEFAULT KEY.

    CONSTANTS co_class_name             TYPE seoclsname VALUE 'CL_ABAPGIT_RES_REPOS' ##NO_TEXT.
    CONSTANTS co_resource_type TYPE string     VALUE 'REPOS' ##NO_TEXT.             "EC NOTEXT
    CONSTANTS co_st_name_pull           TYPE string     VALUE 'ABAPGIT_ST_REPO_PULL' ##NO_TEXT.
    CONSTANTS co_st_name_post_res       TYPE string     VALUE 'ABAPGIT_ST_REPO_POST_RES'.
    CONSTANTS co_root_name_pull         TYPE string     VALUE 'REPOSITORY' ##NO_TEXT.
    CONSTANTS co_root_name_post_res     TYPE string     VALUE 'OBJECTS'.
    CONSTANTS co_content_type_repo_v1   TYPE string     VALUE 'application/abapgit.adt.repo.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v1  TYPE string     VALUE 'application/abapgit.adt.repos.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_object_v1 TYPE string     VALUE 'application/abapgit.adt.repo.object.v1+xml' ##NO_TEXT.
    CONSTANTS co_st_name_get            TYPE string     VALUE 'ABAPGIT_ST_REPOS' ##NO_TEXT.
    CONSTANTS co_root_name_get          TYPE string     VALUE 'REPOSITORIES' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v2   TYPE string     VALUE 'application/abapgit.adt.repo.v2+xml' ##NO_TEXT.

    METHODS post REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    " DATA mo_application_log TYPE REF TO if_a4c_logger .

    METHODS validate_request_data
      IMPORTING
        !is_request_data TYPE ty_request_pull_data
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_res_repo_pull IMPLEMENTATION.


  METHOD post.

    DATA:
      ls_request_data  TYPE ty_request_pull_data,
      result_request   TYPE sadt_status_message,
      lv_repo_key      TYPE zif_abapgit_persistence=>ty_value,
      lo_job_scheduler TYPE REF TO if_cbo_job_scheduler,
      lo_job_action    TYPE REF TO if_cbo_job_action.

    TRY.
        " TODO: implement with valilla ABAPGIT
        ZCX_ABAPGIT_EXCEPTION=>raise( 'Pull is not yet implemented' ).

*------ Get Repository Key
        request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                    IMPORTING value = lv_repo_key ).

*------ Content Handler
        DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
            request         = request
            content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                  st_name      = co_st_name_pull
                                  root_name    = co_root_name_pull
                                  content_type = co_content_type_repo_v1 ) ).

        " zcl_abapgit_operation_log=>clear( ).

*------ Retrieve request data
        request->get_body_data(
          EXPORTING
            content_handler = lo_request_content_handler
          IMPORTING
            data            = ls_request_data ).

*        "Prerequisite check for request values
*        validate_request_data( ls_request_data ).

*NEW start
**------ Create new application logger object
*        DATA ls_header TYPE if_a4c_logger=>ts_header.
*        ls_header-scope       = if_a4c_logger=>cs_scope-agit.
*        ls_header-external_id = cl_system_uuid=>create_uuid_c32_static( ).
*        mo_application_log    = cl_a4c_logger_factory=>get_new_logger( ls_header ).
*
**------ Create new log in history table before starting batch processing
*        DATA(lo_log_factory) = zcl_abapgit_app_log_factory=>get_instance( ).
*        DATA(lo_log) = lo_log_factory->create_new( iv_repo_key    = lv_repo_key
*                                                   iv_repo_branch = ls_request_data-branch ).
*        lo_log->save( ).
*
*        DATA ls_alog_key TYPE tsa4c_agit_applog_key.
*        ls_alog_key-app_log  = lo_log->get_data( )-app_log.
*
**------ Execute background job
*        lo_job_scheduler = NEW cl_cbo_job_scheduler( ).
*
*        lo_job_action = NEW zcl_abapgit_repo_pull_action(
*          is_alog_key = ls_alog_key
*          iv_repo_key = lv_repo_key
*          is_req_data = ls_request_data ).
*
*        "create new job from action
*        lo_job_scheduler->start_job(
*          EXPORTING
*            io_action  = lo_job_action
*          IMPORTING
*            es_job_key = DATA(ls_job_key) ).
*
**        "persist jobname/jobcount in application log table -> DONE inside
**        lo_log->set_batch_job( jobname = ls_job_key-job_name jobcount = ls_job_key-job_count ).
**        lo_log->save( ).
*NEW end

*OLD start
*------ Set log-on information if supplied
        IF ls_request_data-user IS NOT INITIAL AND ls_request_data-password IS NOT INITIAL.
          zcl_abapgit_default_auth_info=>refresh( ).
          zcl_abapgit_default_auth_info=>set_auth_info( iv_user = ls_request_data-user
                                                       iv_password = ls_request_data-password ).
        ENDIF.

*------ Set the default transport request
        zcl_abapgit_default_transport=>get_instance( )->set( CONV #( ls_request_data-transportrequest ) ).

*------ Create online repo
        DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).
        lo_repo->refresh( ).

        DATA(ls_checks) = lo_repo->deserialize_checks( ).

*------ Settings to overwrite existing objects
        LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
          <ls_overwrite>-decision = 'Y'.
        ENDLOOP.

        LOOP AT ls_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
          <ls_warning_package>-decision = 'Y'.
        ENDLOOP.

        ls_checks-transport-transport = ls_request_data-transportrequest.

*------ Import Objects
        lo_repo->deserialize( is_checks = ls_checks ).

        "response content handler
        DATA(lo_resp_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
          st_name      = co_st_name_post_res
          root_name    = co_root_name_post_res
          content_type = co_content_type_object_v1 ).

*------ prepare response information
        " DATA(lt_result_table) = zcl_abapgit_operation_log=>get_result_table( ).

        " response->set_body_data(
        "   content_handler = lo_resp_content_handler
        "   data            = lt_result_table ).
*OLD end

*NEW start
*        response->set_status( cl_rest_status_code=>gc_success_accepted ).
*NEW end

*---- Handle issues
*      CATCH zcx_abapgit_exception zcx_abapgit_app_log cx_cbo_job_scheduler cx_uuid_error INTO DATA(lx_exception).
      CATCH zcx_abapgit_exception cx_cbo_job_scheduler cx_uuid_error INTO DATA(lx_exception).
        ROLLBACK WORK.
        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD validate_request_data.

*    "check whether git url is well formed
*    zcl_abapgit_url=>validate( |{ is_request_data-url }| ).
*
*    "check whether package is already used
*    zcl_abapgit_repo_srv=>get_instance( )->validate_package( CONV #( is_request_data-package ) ).
*
*    "check whether git url is already used
*    DATA(lt_repo_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
*    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<ls_repo_list>).
*      IF cl_http_utility=>if_http_utility~unescape_url( zcl_abapgit_url=>name( is_request_data-url ) ) EQ <ls_repo_list>->get_name( ).
*        MESSAGE e002(A4C_AGIT_ADT) WITH is_request_data-url <ls_repo_list>->get_package( ) INTO DATA(lv_msg).
*        zcx_abapgit_exception=>raise_t100( ).
*      ENDIF.
*    ENDLOOP.
*
*    "transport request exists
*    SELECT SINGLE * FROM e070 INTO @DATA(ls_e070)
*      WHERE
*      trkorr = @is_request_data-transportrequest.
*
*    IF sy-subrc NE 0.
*      MESSAGE e003(A4C_AGIT_ADT) WITH is_request_data-transportrequest INTO lv_msg.
*      zcx_abapgit_exception=>raise_t100( ).
*    ELSEIF ls_e070-trstatus NE 'D'.
*      MESSAGE e004(A4C_AGIT_ADT) WITH is_request_data-transportrequest INTO lv_msg.
*      zcx_abapgit_exception=>raise_t100( ).
*    ELSEIF ls_e070-as4user NE sy-uname.
*      MESSAGE e005(A4C_AGIT_ADT) WITH is_request_data-transportrequest sy-uname INTO lv_msg.
*      zcx_abapgit_exception=>raise_t100( ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
