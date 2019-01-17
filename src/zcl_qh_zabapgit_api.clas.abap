CLASS zcl_qh_zabapgit_api DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .
    INTERFACES zif_abapgit_repo_srv .

    METHODS change_branch
      IMPORTING
        !iv_branch TYPE string
        !iv_repo   TYPE string
      EXPORTING
        !ex_subrc  TYPE subrc .
    METHODS get_repo_list .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_list TYPE zif_abapgit_definitions=>ty_repo_ref_tt .

    METHODS create_background_job
      IMPORTING
        !iv_repo  TYPE string
      EXPORTING
        !ex_subrc TYPE subrc .
ENDCLASS.



CLASS zcl_qh_zabapgit_api IMPLEMENTATION.


  METHOD change_branch.
    DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_online,
          ls_branch TYPE zif_abapgit_definitions=>ty_git_branch,
          lv_branch TYPE zif_abapgit_persistence=>ty_repo-branch_name,
          ls_data   TYPE REF TO data,
          lo_object TYPE REF TO zcl_abapgit_repo,
          lv_key    TYPE zif_abapgit_persistence=>ty_value.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF mt_list,
                   <ls_data> TYPE LINE OF zif_abapgit_persistence=>tt_repo.
    TRY.
* Loop through all the repositories
        LOOP AT mt_list ASSIGNING <ls_list>.
* The class ZCL_ABAPGIT_REPO has all applicable methods.
          IF <ls_list>->get_name(  ) = iv_repo.
            EXIT.
          ENDIF.
        ENDLOOP.
* Get the repository key
        lv_key = <ls_list>->get_key(  ).
        lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
* Make sure to refresh the repository before changing the branch.
        lo_repo->refresh(  ).
        CONCATENATE 'refs/heads/' iv_branch INTO lv_branch.
        lo_repo->set_branch_name( lv_branch ).
        ex_subrc = 0.

      CATCH zcx_abapgit_exception.

    ENDTRY.



    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD create_background_job.


    DATA: p_jobcnt     TYPE tbtcjob-jobcount,
          l_release(1) TYPE c.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = 'ZABAPGIT'
      IMPORTING
        jobcount         = p_jobcnt
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
*MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ex_subrc = sy-subrc.
      RETURN.
    ELSE.
      SUBMIT zabapgit


  AND RETURN

  VIA JOB 'ZABAPGIT'

  NUMBER p_jobcnt.



      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = p_jobcnt
          jobname              = 'ZABAPGIT'
          strtimmed            = 'X'
        IMPORTING
          job_was_released     = l_release
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          OTHERS               = 9.
      sy-subrc = sy-subrc.

    ENDIF.

    ex_subrc = 0.


  ENDMETHOD.


  METHOD get_repo_list.
    DATA: lt_list    TYPE zif_abapgit_persistence=>tt_repo,
          lo_online  TYPE REF TO zcl_abapgit_repo_online,
          lo_offline TYPE REF TO zcl_abapgit_repo_offline.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.

    lt_list = zcl_abapgit_persist_factory=>get_repo( )->list( ).

    LOOP AT lt_list ASSIGNING <ls_list>.
      IF <ls_list>-offline = abap_false.
        CREATE OBJECT lo_online
          EXPORTING
            is_data = <ls_list>.
        APPEND lo_online TO mt_list.
      ELSE.
        CREATE OBJECT lo_offline
          EXPORTING
            is_data = <ls_list>.
        APPEND lo_offline TO mt_list.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    DATA:
      lo_http_client TYPE REF TO if_http_client,
      lv_service     TYPE string,
      ls_input       TYPE string,
      ls_location    TYPE string,
      lv_branch      TYPE string,
      ls_encoding    TYPE string,
      ls_output      TYPE string,
      ls_reason      TYPE string,
      l_key(44)      TYPE c,
      lv_subrc       TYPE subrc,
      lv_statuscode  TYPE i.


    ls_encoding = TEXT-003.
    server->response->set_content_type(
           ls_encoding ).

* Initiliaze the repos
    CALL METHOD me->get_repo_list( ).
*   What type of request is this?

    DATA(lv_action) = server->request->get_form_field( 'action' ) ##NO_TEXT .
    DATA(lv_repo) = server->request->get_form_field( 'repository' ) ##NO_TEXT .
    TRANSLATE lv_action TO UPPER CASE.

* Let's not trust anyone and assume a bad outcome always
    lv_subrc = 4.

    IF lv_action = 'BACKGROUND' AND lv_repo IS NOT INITIAL.
      CALL METHOD me->create_background_job
        EXPORTING
          iv_repo  = lv_repo
        IMPORTING
          ex_subrc = lv_subrc.
    ELSEIF lv_action = 'BRANCH' AND lv_repo IS NOT INITIAL.
      lv_branch = server->request->get_form_field( 'branch' ) ##NO_TEXT .
      IF lv_branch IS NOT INITIAL.
        CALL METHOD me->change_branch
          EXPORTING
            iv_branch = lv_branch
            iv_repo   = lv_repo
          IMPORTING
            ex_subrc  = lv_subrc.
      ELSEIF lv_action = 'PULL' AND lv_repo IS NOT INITIAL.

*        CALL METHOD me->pull
*          EXPORTING
*            iv_repo   = lv_repo
*          IMPORTING
*            ex_subrc  = lv_subrc.
      ELSE.
        lv_subrc = 4.
      ENDIF.
    ENDIF.
    IF lv_subrc = 0.
      lv_statuscode = 200.
    ELSE.
      lv_statuscode = 404.
    ENDIF.

    ls_reason = TEXT-004.
    server->response->set_status(
       code = lv_statuscode
       reason = ls_reason ).
  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~list.

    DATA: lt_repo        TYPE zif_abapgit_definitions=>ty_repo_ref_tt,
          lo_repo        TYPE REF TO zcl_abapgit_repo,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lo_repo_online TYPE REF TO zcl_abapgit_repo_online,
          lv_err         TYPE string.

    lt_repo = zif_abapgit_repo_srv~list( ).
  ENDMETHOD.
ENDCLASS.
