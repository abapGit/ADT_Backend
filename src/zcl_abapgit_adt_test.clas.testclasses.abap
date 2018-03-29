*"* use this source file for your ABAP unit test classes

CLASS ltcl_test DEFINITION INHERITING FROM zcl_abapgit_adt_test FOR TESTING
    RISK LEVEL HARMLESS DURATION SHORT.
* todo, change RISK LEVEL?

  PUBLIC SECTION.
    METHODS: scenario01 FOR TESTING.

    CONSTANTS: c_prefix TYPE string VALUE '/sap/bc/adt/abapgit/'.

    METHODS:
      create_repo
        RETURNING
          VALUE(rv_key) TYPE zif_abapgit_persistence=>ty_repo-key,
      delete_repo
        IMPORTING
          iv_key TYPE zif_abapgit_persistence=>ty_repo-key,
      check_response
        IMPORTING
          is_response TYPE ty_response,
      read_repo
        IMPORTING
          iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        RETURNING
          VALUE(rv_key) TYPE zif_abapgit_persistence=>ty_repo-key,
      repo_status
        IMPORTING
          iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        RETURNING
          VALUE(rv_key) TYPE zif_abapgit_persistence=>ty_repo-key,
      repo_pull
        IMPORTING
          iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        RETURNING
          VALUE(rv_key) TYPE zif_abapgit_persistence=>ty_repo-key.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD scenario01.
    delete_repo( repo_pull( repo_status( read_repo( create_repo( ) ) ) ) ).
  ENDMETHOD.

  METHOD check_response.

    cl_abap_unit_assert=>assert_equals(
      act = is_response-status
      exp = 200
      msg = is_response-body ).

  ENDMETHOD.

  METHOD create_repo.

    DATA: lv_body TYPE string,
          ls_repo TYPE zif_abapgit_persistence=>ty_repo.


    DATA(ls_create) = VALUE zcl_abapgit_adt=>ty_create_repository(
      url         = 'https://github.com/larshp/DOMA.git'
      branch_name = 'refs/heads/master'
      package     = '$ONLINE' ).

    CALL TRANSFORMATION id SOURCE root = ls_create RESULT XML lv_body.

    DATA(ls_response) = post(
      iv_uri  = c_prefix && 'repos'
      iv_body = lv_body ).

    check_response( ls_response ).

    CALL TRANSFORMATION id SOURCE XML ls_response-body RESULT root = ls_repo.
    cl_abap_unit_assert=>assert_not_initial( ls_repo-key ).

    rv_key = ls_repo-key.

  ENDMETHOD.

  METHOD read_repo.

    DATA: ls_repo TYPE zif_abapgit_persistence=>ty_repo.


    DATA(ls_response) = get( c_prefix && 'repos/' && iv_key ).

    check_response( ls_response ).

    CALL TRANSFORMATION id SOURCE XML ls_response-body RESULT root = ls_repo.

    cl_abap_unit_assert=>assert_equals(
      act = ls_repo-key
      exp = iv_key ).

    rv_key = iv_key.

  ENDMETHOD.

  METHOD delete_repo.

    DATA(ls_response) = delete( c_prefix && 'repos/' && iv_key ).

    check_response( ls_response ).

  ENDMETHOD.

  METHOD repo_status.

    DATA: lt_status TYPE zif_abapgit_definitions=>ty_results_tt.


    DATA(ls_response) = get( c_prefix && 'repos/' && iv_key && '/status' ).

    check_response( ls_response ).

    CALL TRANSFORMATION id SOURCE XML ls_response-body RESULT root = lt_status.

    cl_abap_unit_assert=>assert_not_initial( lt_status ).

    rv_key = iv_key.

  ENDMETHOD.

  METHOD repo_pull.

    DATA: lt_status TYPE zif_abapgit_definitions=>ty_results_tt.


    DATA(ls_response) = post( c_prefix && 'repos/' && iv_key && '/pull' ).

    check_response( ls_response ).

    rv_key = iv_key.

  ENDMETHOD.

ENDCLASS.
