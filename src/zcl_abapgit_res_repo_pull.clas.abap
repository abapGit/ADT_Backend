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
    TYPES:
      BEGIN OF ty_repo_w_links.
        INCLUDE TYPE zif_abapgit_persistence=>ty_repo.
    TYPES:   links TYPE if_atom_types=>link_t.
    TYPES: END OF ty_repo_w_links.
    TYPES:
      tt_repo_w_links TYPE STANDARD TABLE OF ty_repo_w_links WITH DEFAULT KEY.

    CONSTANTS co_class_name             TYPE seoclsname VALUE 'ZCL_ABAPGIT_RES_REPOS' ##NO_TEXT.
    CONSTANTS co_resource_type TYPE string     VALUE 'REPOS' ##NO_TEXT.             "EC NOTEXT
    CONSTANTS co_st_name_pull           TYPE string     VALUE 'ZABAPGIT_ST_REPO_PULL' ##NO_TEXT.
    CONSTANTS co_st_name_post_res       TYPE string     VALUE 'ZABAPGIT_ST_REPO_POST_RES'.
    CONSTANTS co_root_name_pull         TYPE string     VALUE 'REPOSITORY' ##NO_TEXT.
    CONSTANTS co_root_name_post_res     TYPE string     VALUE 'OBJECTS'.
    CONSTANTS co_content_type_repo_v1   TYPE string     VALUE 'application/abapgit.adt.repo.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v1  TYPE string     VALUE 'application/abapgit.adt.repos.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_object_v1 TYPE string     VALUE 'application/abapgit.adt.repo.object.v1+xml' ##NO_TEXT.
    CONSTANTS co_st_name_get            TYPE string     VALUE 'ZABAPGIT_ST_REPOS' ##NO_TEXT.
    CONSTANTS co_root_name_get          TYPE string     VALUE 'REPOSITORIES' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v2   TYPE string     VALUE 'application/abapgit.adt.repo.v2+xml' ##NO_TEXT.

    METHODS post REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS validate_request_data
      IMPORTING
        !is_request_data TYPE ty_request_pull_data
        iv_repo_key      TYPE zif_abapgit_persistence=>ty_value
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_res_repo_pull IMPLEMENTATION.


  METHOD post.
    TYPES:
      BEGIN OF t_obj_result,
        obj_type   TYPE trobjtype,
        obj_name   TYPE sobj_name,
        obj_status TYPE symsgty,
        package    TYPE devclass,
        msg_type   TYPE symsgty,
        msg_text   TYPE string,
      END OF t_obj_result.
    DATA lt_result_table TYPE STANDARD TABLE OF t_obj_result WITH DEFAULT KEY.
    DATA:
      ls_request_data  TYPE ty_request_pull_data,
      lv_repo_key      TYPE zif_abapgit_persistence=>ty_value.
    DATA lo_log TYPE REF TO zcl_abapgit_log.

    TRY.

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

*------ Retrieve request data
        request->get_body_data(
          EXPORTING
            content_handler = lo_request_content_handler
          IMPORTING
            data            = ls_request_data ).

        "Prerequisite check for request values
        validate_request_data( is_request_data = ls_request_data iv_repo_key = lv_repo_key ).

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
        " lo_job_scheduler = NEW cl_cbo_job_scheduler( ).
*
        "  lo_job_action = NEW zcl_abapgit_repo_pull_action(
        "     is_alog_key = ls_alog_key
        "     iv_repo_key = lv_repo_key
        "     is_req_data = ls_request_data ).
*
*        "create new job from action
        " lo_job_scheduler->start_job(
        "   EXPORTING
        "     io_action  = lo_job_action
        "   IMPORTING
        "     es_job_key = DATA(ls_job_key) ).
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
        IF ls_request_data-transportrequest <> ''.
          zcl_abapgit_default_transport=>get_instance( )->set( CONV #( ls_request_data-transportrequest ) ).
        ENDIF.

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

        lo_log = NEW #( ).

*------ Import Objects
        lo_repo->deserialize( is_checks = ls_checks ii_log = lo_log ).

        "response content handler
        DATA(lo_resp_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
          st_name      = co_st_name_post_res
          root_name    = co_root_name_post_res
          content_type = co_content_type_object_v1 ).

*------ prepare response information
        response->set_body_data(
          content_handler = lo_resp_content_handler
          data            = lt_result_table ).
*OLD end

*NEW start
*        response->set_status( cl_rest_status_code=>gc_success_accepted ).
*NEW end

*---- Handle issues
      CATCH zcx_abapgit_exception cx_uuid_error INTO DATA(lx_exception).
        ROLLBACK WORK.
        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD validate_request_data.
    DATA: lv_msg TYPE string.

    " only check transport if package requires a transport
    DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( iv_repo_key ).

    DATA(lv_package_key) = lo_repo->get_package( ).

    DATA(lo_package) = zcl_abapgit_factory=>get_sap_package( lv_package_key ).

    IF lo_package->are_changes_recorded_in_tr_req( ) = abap_true.

      "transport request exists
      SELECT SINGLE * FROM e070 INTO @DATA(ls_e070)
        WHERE
        trkorr = @is_request_data-transportrequest.

      IF sy-subrc NE 0.
        lv_msg = |Transport { is_request_data-transportrequest } not found|.
        zcx_abapgit_exception=>raise( lv_msg ).
      ELSEIF ls_e070-trstatus NE 'D'.
        lv_msg = |Transport { is_request_data-transportrequest } is released or protected|.
        zcx_abapgit_exception=>raise( lv_msg ).
      ELSEIF ls_e070-as4user NE sy-uname.
        lv_msg = |Transport { is_request_data-transportrequest } is not owned by { sy-uname }|.
        zcx_abapgit_exception=>raise( lv_msg ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
