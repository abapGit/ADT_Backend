*"* use this source file for your ABAP unit test classes

CLASS ltcl_test DEFINITION INHERITING FROM zcl_abapgit_adt_test FOR TESTING
    RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
    METHODS: create FOR TESTING.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD create.

    DATA: lv_body TYPE string,
          ls_repo TYPE zif_abapgit_persistence=>ty_repo.


    DATA(ls_create) = VALUE zcl_abapgit_adt=>ty_create_repository(
      url         = 'https://github.com/larshp/DOMA.git'
      branch_name = 'refs/heads/master'
      package     = '$ONLINE' ).

    CALL TRANSFORMATION id SOURCE root = ls_create RESULT XML lv_body.

    DATA(ls_response) = post(
      iv_uri  = '/sap/bc/adt/abapgit/repos'
      iv_body = lv_body ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_response-status
      exp = 200
      msg = ls_response-body ).

    CALL TRANSFORMATION id SOURCE XML ls_response-body RESULT root = ls_repo.

    cl_abap_unit_assert=>assert_not_initial( ls_repo-key ).

  ENDMETHOD.

ENDCLASS.
