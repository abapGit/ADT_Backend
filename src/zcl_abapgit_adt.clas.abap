CLASS zcl_abapgit_adt DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_create_repository,
        url         TYPE string,
        branch_name TYPE string,
        package     TYPE devclass,
      END OF ty_create_repository .

    CONSTANTS c_transformation TYPE cxsltdesc VALUE 'ID' ##NO_TEXT.
    CONSTANTS c_uri_key TYPE string VALUE 'key' ##NO_TEXT.

    METHODS delete
        REDEFINITION .
    METHODS get
        REDEFINITION .
    METHODS post
        REDEFINITION .
    METHODS put
        REDEFINITION .
  PROTECTED SECTION.
private section.

  data MI_RESPONSE type ref to IF_ADT_REST_RESPONSE .
  data MI_REQUEST type ref to IF_ADT_REST_REQUEST .

  methods CREATE_REPOSITORY
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_ADT_REST
      ZCX_ABAPGIT_NOT_FOUND .
  methods LIST_REPOSITORIES
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_ADT_REST .
  methods DELETE_REPOSITORY
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_ADT_REST
      ZCX_ABAPGIT_NOT_FOUND .
  methods READ_REPOSITORY
    importing
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-KEY
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_ADT_REST
      ZCX_ABAPGIT_NOT_FOUND .
  methods REPOSITORY_PULL
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_ADT_REST
      ZCX_ABAPGIT_NOT_FOUND .
  methods REPOSITORY_PULL_CHECKS
    importing
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-KEY
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_ADT_REST
      ZCX_ABAPGIT_NOT_FOUND .
  methods REPOSITORY_STATUS
    importing
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-KEY
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_ADT_REST
      ZCX_ABAPGIT_NOT_FOUND .
  methods SET_BODY
    importing
      !IG_DATA type DATA
    raising
      CX_ADT_REST .
  methods SET_ERROR
    importing
      !II_MESSAGE type ref to IF_MESSAGE
      !IV_STATUS type I default 500
    raising
      CX_ADT_REST .
ENDCLASS.



CLASS ZCL_ABAPGIT_ADT IMPLEMENTATION.


  METHOD create_repository.

    DATA: ls_create TYPE ty_create_repository.


    mi_request->get_body_data(
      EXPORTING
        content_handler = cl_adt_rest_st_handler=>create_instance( c_transformation )
      IMPORTING
        data            = ls_create ).

    DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url         = ls_create-url
      iv_branch_name = ls_create-branch_name
      iv_package     = ls_create-package ).

    read_repository( lo_repo->get_key( ) ).

  ENDMETHOD.


  METHOD delete.

    mi_response = response.
    mi_request = request.

    TRY.
        delete_repository( ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        set_error( lx_error ).
      CATCH zcx_abapgit_not_found INTO DATA(lx_not_found).
        set_error( iv_status  = 404
                   ii_message = lx_not_found ).
    ENDTRY.

  ENDMETHOD.


  METHOD delete_repository.

    DATA: lv_string TYPE string.


* todo, use CONTEXT instead?
    mi_request->get_uri_attribute(
      EXPORTING
        name      = c_uri_key
        mandatory = abap_false
      IMPORTING
        value     = lv_string ).

    zcl_abapgit_repo_srv=>get_instance( )->get( |{ lv_string ALPHA = IN }| )->delete( ).

  ENDMETHOD.


  METHOD get.

    DATA: lv_string TYPE string.


    mi_response = response.
    mi_request = request.

* todo, use CONTEXT instead?
    request->get_uri_attribute(
      EXPORTING
        name      = c_uri_key
        mandatory = abap_false
      IMPORTING
        value     = lv_string ).

    DATA(lv_path) = request->get_inner_rest_request( )->get_uri_path( ).

    TRY.
        IF lv_path CP '*/status'.
          repository_status( |{ lv_string ALPHA = IN }| ).
        ELSEIF lv_path CP '*/pull_checks'.
          repository_pull_checks( |{ lv_string ALPHA = IN }| ).
        ELSEIF NOT lv_string IS INITIAL.
          read_repository( |{ lv_string ALPHA = IN }| ).
        ELSE.
          list_repositories( ).
        ENDIF.
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        set_error( lx_error ).
      CATCH zcx_abapgit_not_found INTO DATA(lx_not_found).
        set_error( iv_status  = 404
                   ii_message = lx_not_found ).
    ENDTRY.

  ENDMETHOD.


  METHOD list_repositories.

* todo, use this instead,
*      DATA(lt_repos) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    DATA(lt_repos) = NEW zcl_abapgit_persistence_repo( )->list( ).

    set_body( lt_repos ).

  ENDMETHOD.


  METHOD post.

    mi_response = response.
    mi_request = request.

    DATA(lv_path) = request->get_inner_rest_request( )->get_uri_path( ).

    TRY.
        IF lv_path CP '*/pull'.
          repository_pull( ).
        ELSE.
          create_repository( ).
        ENDIF.
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        set_error( lx_error ).
      CATCH zcx_abapgit_not_found INTO DATA(lx_not_found).
        set_error( iv_status  = 404
                   ii_message = lx_not_found ).
    ENDTRY.

  ENDMETHOD.


  METHOD put.

    mi_response = response.
    mi_request = request.

* todo

*      CATCH zcx_abapgit_exception zcx_abapgit_not_found INTO DATA(lx_error).
*        set_error( lx_error ).
*    ENDTRY.

  ENDMETHOD.


  METHOD read_repository.

* todo, use ZCL_ABAPGIT_REPO_SRV instead of persistence

    DATA(ls_repo) = NEW zcl_abapgit_persistence_repo( )->read( iv_key ).

    set_body( ls_repo ).

  ENDMETHOD.


  METHOD repository_pull.

    DATA: lv_string TYPE string,
          ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.


* todo, use CONTEXT instead?
    mi_request->get_uri_attribute(
      EXPORTING
        name      = c_uri_key
        mandatory = abap_false
      IMPORTING
        value     = lv_string ).

    mi_request->get_body_data(
      EXPORTING
        content_handler = cl_adt_rest_st_handler=>create_instance( c_transformation )
      IMPORTING
        data            = ls_checks ).

    zcl_abapgit_repo_srv=>get_instance( )->get( |{ lv_string ALPHA = IN }| )->deserialize( ls_checks ).

  ENDMETHOD.


  METHOD repository_pull_checks.

    DATA(ls_checks) = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->deserialize_checks( ).

    set_body( ls_checks ).

  ENDMETHOD.


  METHOD repository_status.

    DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

* todo, also supply LOG?
    DATA(lt_result) = zcl_abapgit_file_status=>status( lo_repo ).

    set_body( lt_result ).

  ENDMETHOD.


  METHOD set_body.

    mi_response->set_body_data(
      content_handler = cl_adt_rest_st_handler=>create_instance( c_transformation )
      data            = ig_data ).

  ENDMETHOD.


  METHOD set_error.

    mi_response->set_status( iv_status ).

    mi_response->set_body_data(
      content_handler = NEW cl_adt_rest_plain_text_handler( )
      data            = ii_message->get_text( ) ).

  ENDMETHOD.
ENDCLASS.
