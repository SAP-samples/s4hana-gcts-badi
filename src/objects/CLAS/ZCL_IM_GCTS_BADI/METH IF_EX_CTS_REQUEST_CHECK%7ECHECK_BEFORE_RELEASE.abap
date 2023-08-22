  method if_ex_cts_request_check~check_before_release.
    data: ls_tadir       type tadir,
          ls_e071        type e071,
          lt_e071        type table of e071 with default key,
          lt_packages    type table of devclass,
          lt_tadir       type tadir,
          lt_object_list type ty_tadir,
          itab           type tr_objects,
          ty_request     type if_cts_abap_vcs_api_facade=>ty_push_objects_request,
          ls_address     type bapiaddr3,
          lv_objname     type sobj_name,
          lv_check       type flag,
          logger         type ref to if_cts_abap_vcs_logger,
          action         type string.

    data: repository_request type if_cts_abap_vcs_api_facade=>ty_get_repositories_request.
    logger ?= new cl_cts_abap_vcs_logger( section = 'GIT_FORWARD_BADI_TEST' ).
    action = 'CHECK_BEFORE_RELEASE'.
    logger->log_start( action = action info = |User: { sy-uname }| ).
    logger->log_info( action = action info = |Transport request: { request }| ).
    if tarsystem is initial.
*       The target system is initial, then the parent transport request target system is retrieved.
      logger->log_info( action = action info = 'Retrieving the transport request for the task').
      select single * from e070 into @data(ls_e070) where trkorr = @request.
      if sy-subrc = 0.
        if ls_e070-strkorr is not initial.
          logger->log_info( action = action info = |Parent TR : { ls_e070-strkorr } | ).
          select single * from e070 into @data(parent_tr_e070) where trkorr = @ls_e070-strkorr.
          if sy-subrc = 0.
            logger->log_info( action = action info = |Parent TR target system : { parent_tr_e070-tarsystem } | ).
            data(target_system) = parent_tr_e070-tarsystem.
          endif.
        endif.
      endif.
    else.
      target_system = tarsystem.
    endif.
*       R - repair, S - development/correction,  Q -customizing task
    if type = 'R' or type = 'S' or type = 'Q'.
      logger->log_info( action = action info = |Type of Release: { type }| ).
      data(lo_gcts_feature_toggles) = new ltcl_gcts_feature_toggle( ).
      data(ls_toggle_functionalities) = lo_gcts_feature_toggles->set_badi_toggles(  ).
      data(ls_badi_trigger_toggle) = lo_gcts_feature_toggles->get_badi_trigger_toggles(  ).
      data(lo_gcts_general_functions) = ls_toggle_functionalities-general_functions.
      data(lo_badi_trigger_class) = new ltcl_gcts_badi_trigger( badi_trigger_toggle = ls_badi_trigger_toggle-value
                                                          badi_trigger_info = ls_badi_trigger_toggle-additional_info
                                                          target_system = target_system ).

      if lo_badi_trigger_class->check_user_trigger(  ) = abap_true.
        try.
            data(prepared_objects) = lo_gcts_general_functions->prepare_objects_for_push( tr_objects = objects tr_keys = keys ).
          catch cx_cts_abap_vcs_exception into data(exc).
            logger->log_error( action = action info = 'gCTS BAdI Error during preparation phase, please check the application logs').
            logger->flush(  ).
            logger->flush( abap_true ).
            raise cancel.
        endtry.
*     Check if the badi can be triggered based on the toggle and the information set along with the toggle.
*     Otherwise execution of this badi would be ignored.
        lo_badi_trigger_class->set_object_list( it_object_list = prepared_objects ).
        if lo_badi_trigger_class->check_other_badi_triggers(  ) = abap_true.
          logger->log_info( action = action info = |Gcts BAdI execution has been configured to run| ).
          try.
              data(set_repository_response) = lo_gcts_general_functions->set_repository_for_objects( object_list = prepared_objects vsid = conv #( target_system ) ).
            catch cx_cts_abap_vcs_exception into data(cts_exception).
              logger->log_error( action = action info = 'Error when checking objects in registry').
              logger->flush(  ).
              logger->flush( abap_true ).
              raise cancel.
            catch cx_cts_github_api_exception into data(github_exception).
              logger->log_error( action = action info = 'Github Exception').
              logger->flush(  ).
              logger->flush( abap_true ).
              raise cancel.
          endtry.
          data: objects_to_push type ltcl_gcts_general_functions=>tt_gcts_badi_object .
          objects_to_push = set_repository_response-resolved_objects.
*       Check if all the objects are resolved to a repository. else based on the level mentioned in the Relation Identifier
*       additional information different steps are taken.
          if lo_gcts_feature_toggles->get_relation_ident_inf_toggles(  )-level = co_level_error and lines( set_repository_response-unresolved_objects ) > 0.
            logger->log_error( action = action info = 'Error : Not all objects have been resolved to a repository.').
            lo_gcts_general_functions->log_unresolved_objects( unresolved_objects = set_repository_response-unresolved_objects ).
            logger->log_end( action = action ).
            message |Error : Not all objects have been resolved to a repository.| type 'E'.
            raise cancel.
          elseif  lo_gcts_feature_toggles->get_relation_ident_inf_toggles(  )-level = co_level_info and lines( set_repository_response-unresolved_objects ) > 0.
            logger->log_info( action = action info = 'Not all objects have been resolved to a repository. Log Level is Info').
            lo_gcts_general_functions->log_unresolved_objects( unresolved_objects = set_repository_response-unresolved_objects ).
          endif.
          data(unique_repositories) = lo_gcts_general_functions->retreive_unique_repositories( objects_to_push ).
          data: repositories_for_push type ltcl_gcts_general_functions=>tt_unique_repo_object.
          clear repositories_for_push.
          data(lo_gcts_branch_handling) = ls_toggle_functionalities-branch_handling.
          loop at unique_repositories into data(ls_repository).
            logger->log_info( action = action info = |Checking permissions of the user for the repository : { ls_repository-rid }| ).
            try.
                data(lv_target_check) = lo_gcts_general_functions->target_system_check( repository = ls_repository  tarsystem =  target_system ).
                if lv_target_check = abap_false.
                  logger->log_error( action = action info = |Target system of the transport request and the vSID of the repository { ls_repository-rid } do not match| ).
                  logger->log_end( action = action ).
                  logger->flush(  ).
                  logger->flush( abap_true ).
                  message |Target system of the transport request and the vSID of the repository { ls_repository-rid } do not match| type 'E'.
                  raise cancel.
                endif.
              catch cx_cts_abap_vcs_exception into exc.
                logger->log_error( action = action info = 'The target system check failed due to an exception').
                logger->log_error( action = action info = | { exc->get_message(  ) }| ).
                logger->log_end( action = action ).
                logger->flush(  ).
                logger->flush( abap_true ).
                message |The target system check failed due to an exception, check Application Logs in SLG1| type 'E'.
                raise cancel.
            endtry.
            try.
                data(rv_user_check) = lo_gcts_general_functions->authentication_check( ls_repository ).
                logger->log_info( action = action info = |user for remote repository : { rv_user_check }| ).
              catch cx_cts_abap_vcs_exception.
                logger->log_error( action = action info = 'Git user cannot be authenticated').
                logger->log_end( action = action ).
                logger->flush(  ).
                logger->flush( abap_true ).
                message |Git user cannot be authenticated, push not possible| type 'E'.
                raise cancel.
              catch cx_cts_git_api_exception.
                logger->log_error( action = action info = 'Git user cannot be authenticated').
                logger->log_end( action = action ).
                logger->flush(  ).
                logger->flush( abap_true ).
                message |Git user cannot be authenticated, push not possible| type 'E'.
                raise cancel.
            endtry.
            try.
                lo_gcts_general_functions->check_user_permission( ls_repository ).
              catch cx_cts_abap_vcs_exception into data(gcts_exception).
                logger->log_error( action = action info = |Write permission missing for repository { ls_repository-rid } locally | ).
                logger->log_end( action = action ).
                logger->flush(  ).
                logger->flush( abap_true ).
                message |Write permission missing for repository { ls_repository-rid } locally | type 'E'.
                raise cancel.
              catch cx_cts_git_api_exception into data(git_exception).
                logger->log_error( action = action info = |Cannot release task. Write permission for remote repository { ls_repository-rid } required| ).
                logger->log_end( action = action ).
                logger->flush(  ).
                logger->flush( abap_true ).
                message |Cannot release task. Write permission for remote repository { ls_repository-rid } required| type 'E'.
                raise cancel.
            endtry.
*         Do the extra repository evaluation based on the other triggers available.
            data(lo_gcts_repository_evaluation) = ls_toggle_functionalities-repository_evaluation.
            try.
                data(system) = cl_cts_abap_vcs_system_factory=>get_instance( )->get_default_system( ).
                data(lo_repository) = system->get_repository_by_id( ls_repository-rid ).
              catch cx_cts_abap_vcs_exception into gcts_exception.
                logger->log_error( action = action info = 'gCTS Exception').
                logger->log_end( action = action ).
                logger->flush(  ).
                logger->flush( abap_true ).
                message |System info cannot be retrieved| type 'E'.
                raise cancel.
            endtry.
            data(branch_handling_toggle) = lo_gcts_feature_toggles->get_branch_handling_toggles(  ).
            if branch_handling_toggle-value = abap_true.
              data(branch_to_change) = lo_gcts_branch_handling->retreive_branch_info( transport_number = request ).
              if branch_to_change is not initial.
*         Switch or create branch based on the condition set in the maintenance view
                data(branch_change_successful) = lo_gcts_branch_handling->handle_branch_operation( repository_id = ls_repository-rid
                                                                                                   repository = lo_repository
                                                                                                   new_branch = branch_to_change ).
              endif.
              if branch_change_successful = abap_false.
                logger->log_error( action = action info = |Repository branch handling failed for { ls_repository-rid } for branch { branch_to_change } | ).
                logger->log_end( action = action ).
                logger->flush(  ).
                logger->flush( abap_true ).
                message |Repository branch handling failed for { ls_repository-rid }| type 'E'.
                raise cancel.
              endif.
            endif.
            try.
                data(eval_result) = lo_gcts_repository_evaluation->repository_eval_controller( repository = lo_repository target_system = target_system tr_number = request ).
              catch cx_cts_abap_vcs_exception into exc.
                message | { exc->message_variable_1 } for repository { ls_repository-rid } | type 'E'.
                raise cancel.
            endtry.
            if eval_result = abap_false.
              logger->log_error( action = action info = |Repository evaluation failed for { ls_repository-rid } | ).
              logger->log_end( action = action ).
              logger->flush(  ).
              logger->flush( abap_true ).
              message |Repository evaluation failed for { ls_repository-rid }| type 'E'.
              raise cancel.
            else.
              append ls_repository to repositories_for_push.
            endif.
          endloop.
          if lines( repositories_for_push ) > 0.
*         There are repository entry calculated from the previous steps.
*         These repositories would be used to push the objects in the Transport request
            data(ls_response) = lo_gcts_general_functions->commit_and_push_to_repo( objects_to_push = objects_to_push
                                                                                  destination_repository = repositories_for_push
                                                                                  commit_text = text  ).
            if not ls_response-exception is initial.
              logger->log_end( action = action ).
              logger->flush(  ).
              logger->flush( abap_true ).
              message |Error while committing changes| type 'E'.
              raise cancel.
            endif.
          else.
            logger->log_info( action = action info = 'There are no repositories to push to').
          endif.
        endif.
      endif.
    endif.
    logger->flush(  ).
    logger->flush( abap_true ).
    logger->log_end( action = action ).
  endmethod.