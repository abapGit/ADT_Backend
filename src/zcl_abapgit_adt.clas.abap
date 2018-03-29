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
protected section.
private section.

  data MI_RESPONSE type ref to IF_ADT_REST_RESPONSE .

  methods CREATE_REPOSITORY
    importing
      !IS_CREATE type TY_CREATE_REPOSITORY
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_ADT_REST
      ZCX_ABAPGIT_NOT_FOUND .
  methods LIST_REPOSITORIES
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_ADT_REST .
  methods READ_REPOSITORY
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

    DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url         = is_create-url
      iv_branch_name = is_create-branch_name
      iv_package     = is_create-package ).

    read_repository( lo_repo->get_key( ) ).

  ENDMETHOD.


  METHOD delete.

    mi_response = response.

* todo

*      CATCH zcx_abapgit_exception zcx_abapgit_not_found INTO DATA(lx_error).
*        set_error( lx_error ).
*    ENDTRY.

  ENDMETHOD.


  METHOD get.

    DATA: lv_string TYPE string.

    mi_response = response.


* todo, use CONTEXT instead?
    request->get_uri_attribute(
      EXPORTING
        name      = c_uri_key
        mandatory = abap_false
      IMPORTING
        value     = lv_string ).

    TRY.
        IF NOT lv_string IS INITIAL.
          read_repository( |{ lv_string ALPHA = IN }| ).
        ELSE.
          list_repositories( ).
        ENDIF.
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        set_error( lx_error ).
      CATCH zcx_abapgit_not_found INTO DATA(lx_not_found).
        set_error( iv_status = 404
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

    DATA: ls_create TYPE ty_create_repository.

    mi_response = response.


    request->get_body_data(
      EXPORTING
        content_handler = cl_adt_rest_st_handler=>create_instance( c_transformation )
      IMPORTING
        data            = ls_create ).

    TRY.
        create_repository( ls_create ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        set_error( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD put.

    mi_response = response.

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
