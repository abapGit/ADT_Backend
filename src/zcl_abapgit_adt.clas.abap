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

    DATA: lv_string  TYPE string,
          li_handler TYPE REF TO if_adt_rest_content_handler.


    li_handler = cl_adt_rest_st_handler=>create_instance( c_transformation ).

    request->get_uri_attribute(
      EXPORTING
        name      = c_uri_key
        mandatory = abap_false
      IMPORTING
        value     = lv_string ).

    IF NOT lv_string IS INITIAL.
      DATA(ls_repo) = NEW zcl_abapgit_persistence_repo( )->read( |{ lv_string ALPHA = IN }| ).

      response->set_body_data(
        content_handler = li_handler
        data            = ls_repo ).
    ELSE.
      DATA(lt_repos) = NEW zcl_abapgit_persistence_repo( )->list( ).

      response->set_body_data(
        content_handler = li_handler
        data            = lt_repos ).
    ENDIF.

* todo, catch exception

  ENDMETHOD.
ENDCLASS.
