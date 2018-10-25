class ZCL_ABAPGIT_RES_REPO definition
  public
  inheriting from CL_ADT_REST_RESOURCE
  final
  create public .

public section.

  methods DELETE
    redefinition .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_RES_REPO IMPLEMENTATION.


  METHOD delete.

    DATA lv_repo_key TYPE zif_abapgit_persistence=>ty_value.

    request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                IMPORTING value = lv_repo_key ).

    TRY.
        DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).

        zcl_abapgit_repo_srv=>get_instance( )->delete( lo_repo ).
        COMMIT WORK.

        response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH zcx_abapgit_exception INTO DATA(lx_exception).
        zcx_adt_rest_abapgit=>raise_with_error( ix_error = lx_exception
                                               iv_http_status = cl_rest_status_code=>gc_client_error_not_found ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
