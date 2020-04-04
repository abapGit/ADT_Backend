CLASS zcl_abapgit_res_repo_push DEFINITION PUBLIC INHERITING FROM cl_adt_rest_resource FINAL CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF  ty_s_result,
        reason  TYPE string,
        message TYPE string,
      END OF ty_s_result .
    TYPES:
      BEGIN OF ty_request_pull_data,
        branch           TYPE string,
        transportrequest TYPE string,
        user             TYPE string,
        password         TYPE string,
      END OF ty_request_pull_data .
    TYPES:
      BEGIN OF ty_repo_w_links.
        INCLUDE  TYPE zif_abapgit_persistence=>ty_repo.
    TYPES:   links TYPE if_atom_types=>link_t.
    TYPES: END OF ty_repo_w_links .
    TYPES:
      tt_repo_w_links TYPE STANDARD TABLE OF ty_repo_w_links WITH DEFAULT KEY .

* File status
    CONSTANTS co_file_state_added    TYPE string VALUE 'A'.
    CONSTANTS co_file_state_modified TYPE string VALUE 'M'.
    CONSTANTS co_file_state_deleted  TYPE string VALUE 'D'.

    METHODS post
        REDEFINITION .
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_credentials,
             user     TYPE string,
             password TYPE string,
           END OF ty_credentials.
    TYPES: BEGIN OF ty_push_request,
            staged_objects  TYPE zcl_abapgit_res_repo_stage=>tt_abapgit_object, "tta4c_abapgit_object,
             abapgit_comment TYPE zif_abapgit_definitions=>ty_comment, "tsa4c_abapgit_comment,
             credentials     TYPE ty_credentials, "tsa4c_abapgit_credentials,
           END OF ty_push_request.

    METHODS transform_request_data
      IMPORTING
        !is_request_data TYPE zcl_abapgit_res_repo_stage=>ty_abapgit_staging
        !iv_user         TYPE string
        !iv_password     TYPE string
      RETURNING
        VALUE(rs_result) TYPE ty_push_request "tsa4c_abapgit_push_request
      RAISING
        zcx_abapgit_exception .

ENDCLASS.



CLASS ZCL_ABAPGIT_RES_REPO_PUSH IMPLEMENTATION.


  METHOD post.
    DATA:
      lv_repo_key                 TYPE zif_abapgit_persistence=>ty_value,
      lv_username                 TYPE string,
      lv_password                 TYPE string,
      ls_request_data             TYPE zcl_abapgit_res_repo_stage=>ty_abapgit_staging,
      lo_repo_online              TYPE REF TO zcl_abapgit_repo_online,
      ls_commit                   TYPE zif_abapgit_services_git=>ty_commit_fields,
      lo_stage                    TYPE REF TO zcl_abapgit_stage,
      ls_files                    TYPE zif_abapgit_definitions=>ty_stage_files,
      lo_http_utility             TYPE REF TO cl_http_utility,
      ls_request_data_transformed TYPE ty_push_request.

    TRY.
*------ Handle request data -----------*
*------ Get repository key
        request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                    IMPORTING value = lv_repo_key ).

*------ Content Handler
        DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
            request         = request
            content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                  st_name      = zcl_abapgit_res_repo_stage=>co_st_name
                                  root_name    = zcl_abapgit_res_repo_stage=>co_root_name
                                  content_type = zcl_abapgit_res_repo_stage=>co_content_type_v1 ) ).

*------ Retrieve request data
        request->get_body_data(
          EXPORTING
            content_handler = lo_request_content_handler
          IMPORTING
            data            = ls_request_data ).

*------ Get credentials from request header
        CREATE OBJECT lo_http_utility.
        lv_username = request->get_inner_rest_request( )->get_header_field( iv_name = 'Username' ).
*------ Client encodes password with base64 algorithm
        CALL METHOD lo_http_utility->decode_base64
          EXPORTING
            encoded = request->get_inner_rest_request( )->get_header_field( iv_name = 'Password' )
          RECEIVING
            decoded = lv_password.

*------ Determine repo specific data
        " zcl_abapgit_factory=>get_environment( )->set_repo_action( if_abapgit_app_log=>c_action_push ).
        DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).
        lo_repo_online ?= lo_repo.
        DATA(lv_repo_branch) = lo_repo_online->get_branch_name( ).

        GET PARAMETER ID 'A4C_AGIT_PUSH_SYNC' FIELD DATA(lv_push_sync).
*------ Synchronous Processing
        IF lv_username IS NOT INITIAL AND lv_password IS NOT INITIAL.
          zcl_abapgit_default_auth_info=>set_auth_info( iv_user     = lv_username
                                                       iv_password = lv_password ).
        ENDIF.

*------ Force refresh on stage, to make sure the latest local and remote files are used
        lo_repo_online->refresh( ).

*------ Prepare commit fields
        ls_commit-repo_key        = lv_repo_key.
        ls_commit-committer_name  = ls_request_data-abapgit_comment-committer-name.
        ls_commit-committer_email = ls_request_data-abapgit_comment-committer-email.
        ls_commit-author_name     = ls_request_data-abapgit_comment-author-name.
        ls_commit-author_email    = ls_request_data-abapgit_comment-author-email.
        ls_commit-comment         = ls_request_data-abapgit_comment-comment.
        "ls_commit-body            = .

        CREATE OBJECT lo_stage.
        ls_files = zcl_abapgit_factory=>get_stage_logic( )->get( lo_repo_online ).

*------ Create new log in history table before starting batch processing
        " lo_log_factory = zcl_abapgit_app_log_factory=>get_instance( ).
        " lo_log = lo_log_factory->create_new( iv_repo_key    = lv_repo_key
        "                                            iv_repo_branch = lv_repo_branch
        "                                            iv_repo_action   = if_abapgit_app_log=>c_action_push ).
        " lo_log->save( ).

**------ Retrieve repository content
        LOOP AT ls_request_data-staged_objects ASSIGNING FIELD-SYMBOL(<ls_staged_objects>).
          LOOP AT <ls_staged_objects>-files ASSIGNING FIELD-SYMBOL(<ls_staged_objects_files>).
            IF <ls_staged_objects_files>-localstate = co_file_state_added
            OR <ls_staged_objects_files>-localstate = co_file_state_modified.
*-------------- New file or changed file
              LOOP AT ls_files-local ASSIGNING FIELD-SYMBOL(<ls_files_local>).
                IF <ls_files_local>-file-filename EQ <ls_staged_objects_files>-filename.
                  lo_stage->add( iv_path     = <ls_files_local>-file-path
                                 iv_filename = <ls_files_local>-file-filename
                                 iv_data     = <ls_files_local>-file-data ).
                  " lo_log->add_text( iv_text = |File { <ls_files_local>-file-filename } of object {
                  " <ls_staged_objects>-object_ref-type } { <ls_staged_objects>-object_ref-name
                  "} is selected to be exported |
                  "                   iv_type = 'I' ).
                  CONTINUE.
                ENDIF.
              ENDLOOP.
            ELSEIF <ls_staged_objects_files>-localstate = co_file_state_deleted.
*-------------- Deletion case: file need to be deleted on repository
              lo_stage->rm( iv_path     = <ls_staged_objects_files>-path
                            iv_filename = <ls_staged_objects_files>-filename ).
              " lo_log->add_text( iv_text = |File { <ls_staged_objects_files>-filename } of object {
              " <ls_staged_objects>-object_ref-type } { <ls_staged_objects>-object_ref-name
              "} is selected to be removed remotely |  iv_type = 'I' ).
            ENDIF.
          ENDLOOP.
        ENDLOOP.

*------ Trigger commit
        zcl_abapgit_services_git=>commit( is_commit   = ls_commit
                                         io_repo     = lo_repo_online
                                         io_stage    = lo_stage ).

*-------- Determine Log status
        " DATA(lv_run_status) = lo_log->if_abapgit_log~get_status( ).
        " CASE lv_run_status.
        "   WHEN if_abapgit_app_log=>c_run_status-success.
        "     lo_log->add_text( iv_text = 'Objects pushed successfully'  iv_type = 'S' ).
        "   WHEN if_abapgit_app_log=>c_run_status-warning.
        "     lo_log->add_text( iv_text = 'Objects pushed with warnings' iv_type = 'W' ).
        "   WHEN OTHERS. "no other value expected
        "     lo_log->add_text( iv_text = 'Objects pushed with error(s)' iv_type = 'E' ).
        " ENDCASE.
        " lo_log->set_run_status( lv_run_status ).

*------ Prepare Response
        response->set_status( cl_rest_status_code=>gc_success_ok ).

*------ Handle issues
      CATCH zcx_abapgit_cancel zcx_abapgit_exception cx_uuid_error
            zcx_abapgit_not_found INTO DATA(lx_exception).
        " IF lo_log IS BOUND.
        "   lo_log->add_exception( lx_exception ).
        "   lo_log->add_text( iv_text = 'Repository Push aborted' iv_type = 'A' ).
        "   lo_log->set_run_status( if_abapgit_app_log=>c_run_status-aborted ).
        " ENDIF.
        ROLLBACK WORK.
        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD transform_request_data.
    DATA:
      ls_staged_objects_transformed LIKE LINE OF rs_result-staged_objects,
      ls_staged_objects_files       LIKE LINE OF ls_staged_objects_transformed-files.

*--- Node: STAGED_OBJECTS
    LOOP AT is_request_data-staged_objects ASSIGNING FIELD-SYMBOL(<ls_staged_objects>).
      CLEAR: ls_staged_objects_transformed, ls_staged_objects_files.
      ls_staged_objects_transformed-object_ref = <ls_staged_objects>-object_ref.
      LOOP AT <ls_staged_objects>-files ASSIGNING FIELD-SYMBOL(<ls_staged_objects_files>).
        ls_staged_objects_files-filename = <ls_staged_objects_files>-filename.
        ls_staged_objects_files-path = <ls_staged_objects_files>-path.
        ls_staged_objects_files-localstate = <ls_staged_objects_files>-localstate.
        ls_staged_objects_files-remotestate = <ls_staged_objects_files>-remotestate.
        INSERT ls_staged_objects_files INTO TABLE ls_staged_objects_transformed-files.
      ENDLOOP.
      INSERT ls_staged_objects_transformed INTO TABLE rs_result-staged_objects.
    ENDLOOP.

*--- Node: ABAPGIT_COMMENT
    rs_result-abapgit_comment = is_request_data-abapgit_comment.

*--- Node: CREDENTIALS
    rs_result-credentials-user = iv_user.
    rs_result-credentials-password = iv_password.
  ENDMETHOD.
ENDCLASS.
