class ZCL_ABAPGIT_ADT definition
  public
  inheriting from CL_ADT_REST_RESOURCE
  create public .

public section.

  constants C_TRANSFORMATION type CXSLTDESC value 'ID' ##NO_TEXT.
  constants C_URI_KEY type STRING value 'key' ##NO_TEXT.

  methods GET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_ADT IMPLEMENTATION.


  METHOD get.

    DATA: ls_repo    TYPE zif_abapgit_persistence=>ty_repo,
          lv_string  TYPE string,
          li_handler TYPE REF TO if_adt_rest_content_handler.


    request->get_uri_attribute(
      EXPORTING
        name      = c_uri_key
        mandatory = abap_false
      IMPORTING
        value     = lv_string ).

* todo, load data into LS_REPO
    ls_repo-key = lv_string.

    li_handler = cl_adt_rest_st_handler=>create_instance( c_transformation ).

    response->set_body_data(
      content_handler = li_handler
      data            = ls_repo ).

  ENDMETHOD.
ENDCLASS.
