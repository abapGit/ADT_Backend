CLASS zcl_abapgit_adt DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_transformation TYPE cxsltdesc VALUE 'ID' ##NO_TEXT.
    CONSTANTS c_uri_key TYPE string VALUE 'key' ##NO_TEXT.

    TYPES: BEGIN OF ty_create,
             url         TYPE string,
             branch_name TYPE string,
             package     TYPE devclass,
           END OF ty_create.

    METHODS get
        REDEFINITION .
    METHODS post
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_ADT IMPLEMENTATION.


  METHOD get.

    DATA: lv_string  TYPE string.


    DATA(li_handler) = cl_adt_rest_st_handler=>create_instance( c_transformation ).

    request->get_uri_attribute(
      EXPORTING
        name      = c_uri_key
        mandatory = abap_false
      IMPORTING
        value     = lv_string ).

    IF NOT lv_string IS INITIAL.
* todo, use ZCL_ABAPGIT_REPO_SRV instead of persistence
      DATA(ls_repo) = NEW zcl_abapgit_persistence_repo( )->read( |{ lv_string ALPHA = IN }| ).

      response->set_body_data(
        content_handler = li_handler
        data            = ls_repo ).
    ELSE.
* todo, use this instead,
*      DATA(lt_repos) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

      DATA(lt_repos) = NEW zcl_abapgit_persistence_repo( )->list( ).

      response->set_body_data(
        content_handler = li_handler
        data            = lt_repos ).
    ENDIF.

* todo, catch exception

  ENDMETHOD.


  METHOD post.

    DATA: ls_create TYPE ty_create.


    request->get_body_data(
      EXPORTING
        content_handler = cl_adt_rest_st_handler=>create_instance( c_transformation )
      IMPORTING
        data            = ls_create ).

*    zcl_abapgit_repo_srv=>get_instance( )->new_online(
*      iv_url         = ls_create-url
*      iv_branch_name = ls_create-branch_name
*      iv_package     = ls_create-package ).

  ENDMETHOD.
ENDCLASS.
