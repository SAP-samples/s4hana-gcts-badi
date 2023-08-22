*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class ltcl_gcts_general_functions definition.
  public section.
    types:
      begin of ty_repository_objects,
        pgmid    type pgmid,
        obj_type type trobjtype,
        objname  type sobj_name,
      end of ty_repository_objects .
    types:
      begin of ty_vsid_count,
        vsid  type string,
        count type integer,
      end of ty_vsid_count.
    types tt_vsid_count type standard table of ty_vsid_count with default key.

    types tt_tadir type standard table of tadir with default key.
    types ty_objects_to_push type table of if_cts_abap_vcs_transport_req=>ty_objects with default key.
    types tt_unique_repo_object type standard table of if_cts_abap_vcs_repository=>ty_repository_json with default key.

    types: begin of ty_gcts_badi_object,
             object             type if_cts_abap_vcs_transport_req=>ty_objects,
             object_for_wbo     type cl_cts_abap_vcs_organizer_fac=>ty_object,
             dev_class          type devclass,
             gen_flag           type gen_flag,
             is_customizing     type boolean,
             associated_repo    type if_cts_abap_vcs_repository=>ty_repository_json,
             associated_repo_id type scts_abap_vcs_repository_id,
           end of ty_gcts_badi_object.

    types tt_gcts_badi_object type standard table of ty_gcts_badi_object with default key.
    types: begin of ty_resolve_object_response,
             resolved_objects   type tt_gcts_badi_object,
             unresolved_objects type tt_gcts_badi_object,
           end of ty_resolve_object_response.

    types:
      begin    of          ty_repository_objects_cache,
        repository_json    type  if_cts_abap_vcs_repository=>ty_repository_json,
        repository_objects type if_cts_abap_vcs_repository=>tt_objects,
        repository_id      type scts_abap_vcs_repository_id,
      end of ty_repository_objects_cache.

    types tt_repository_objects_cache type standard table of ty_repository_objects_cache with default key.

    class-data: repository_cache_table type tt_repository_objects_cache.

    data: logger  type ref to if_cts_abap_vcs_logger.
    "! Constructor of the class, sets the value for the package, which would be used for filtering out objects for further gcts processes.
    "!
    "! @parameter package     | The name of the package (DEVC) for filtering out objects in a task/request for further gCTS based tasks
    "! @parameter commit_async_toggle     | Toggle for the commit operation. The default value is false, this would mean that the commit would be done in a synchronous way.
    "! If set true, asynchronous way of pushing objects to the remote would be done.
    "! @parameter authentication_toggle  | Toggle to authenticate the user
    "! @parameter user_permission_toggle | Toggle to check if the user has permission to make changes to the remote and local repository
    "! @parameter target_system_check_toggle | Toggle to check if the target system mentioned in the Transport request and the repository VSID are the same
    methods constructor
      importing package                    type string
                commit_async_toggle        type boolean default abap_false
                authentication_toggle      type boolean default abap_false
                user_permission_toggle     type boolean default abap_false
                target_system_check_toggle type boolean default abap_false
                object_resolvement_toggle  type boolean default abap_true
                warning_level_toggle       type boolean default abap_false
                unique_repository          type boolean default abap_false
                badi_trigger               type boolean default abap_false
                object_resolvement_info    type string optional
                warning_level_info         type string optional
                unique_repository_info     type string optional
                badi_trigger_info          type string optional
                object_resolvement_level   type string optional.

*   Constant for logging the entire section.
    constants co_general_functions type string value 'GIT_FORWARD_BADI_GENERAL_FUNCTIONS'.
*   Constants for logging. Each constant is for a certain action/method
    constants: co_check_object_exists        type string value 'CHECK_OBJECT_EXISTS',
               co_retreive_complete_object   type string value 'RETREIVE_COMPLETE_OBJECT',
               co_prepare_object             type string value 'PREPARE_OBJECT',
               co_get_registered_repo        type string value 'GET_REGISTERED_REPO',
               co_commit_and_push_to_repo    type string value 'COMMIT_AND_PUSH_TO_REPO',
               co_prepare_objects_for_push   type string value 'PREPARE_OBJECTS_FOR_PUSH',
               co_set_repository_for_objects type string value 'SET_REPOSITORY_FOR_OBJECTS',
               co_retrieve_unique_repos      type string value 'RETRIEVE UNIQUE REPOSITORIES',
               co_repository_eval_controller type string value 'REPOSITORY EVALUATION CONTROLLER',
               co_log_unresolved_obects      type string value 'LOG_UNRESOLVED_OBJECTS'.

    "! Method to retrieve the complete object when provided with the partial object for example object type 'LIMU'.
    "! The partial object is then used to retrieve the TADIR entry and then the corresponding TADIR entry of the full object is then returned.
    "!
    "! @parameter object     | partial object for which the TADIR entry has to be checked for
    "!
    "! @parameter rs_tadir_object     | Returns the matching TADIR entry (PGMID, object type and object name)
    methods retreive_complete_object
      importing object                 type e071
      returning value(rs_tadir_object) type ty_repository_objects.

    "! Method to retrieve the unique repositories from the object list. This is required for further checks that has to be done before pushing
    "! An example : The user permission has to be checked for all the repositories before it can be pushed
    "!
    "! @parameter object_list     | the badi object list which has the repository field filled.
    "!
    "! @parameter rt_unique_repositories     | Returns the json object structure of all the unique repositories
    methods retreive_unique_repositories
      importing object_list                   type tt_gcts_badi_object
      returning value(rt_unique_repositories) type tt_unique_repo_object.

    "! Method to check if the object exists in the abap system.
    "! The object is checked with the function module 'DEV_CHECK_OBJECT_EXISTS' and a flag value is returned
    "!
    "! @parameter object     | partial object for which the TADIR entry has to be checked for
    "!
    "! @parameter rv_check     | Returns the result whether the object exists in the system or not
    methods check_object_exists
      importing object          type  ty_repository_objects
      returning value(rv_check) type flag .

    "! Add description
    "!
    methods prepare_object
      importing
                object           type  ty_repository_objects
      returning value(rs_object) type tadir
      raising   cx_cts_abap_vcs_exception.

    methods prepare_objects_for_push
      importing
                tr_objects            type tr_objects
                tr_keys               type tr_keys
      returning value(rt_object_list) type tt_gcts_badi_object
      raising   cx_cts_abap_vcs_exception.

    methods set_repository_for_objects
      importing
                object_list                    type tt_gcts_badi_object
                vsid                           type string optional
      returning value(set_repository_response) type ty_resolve_object_response
      raising   cx_cts_abap_vcs_exception.

    "! Method to retreive the repository that has the package registered by using gCTS registry.
    "! The package passed as argument to the method is used to check if it has been registered in any of the repository
    "! If it is registered then the repository object is retreived and returned
    "!
    "! @parameter objects     | objects to be searched in the registry
    "!
    "! @parameter rs_repository     | The tadir entry of the object which matches the filter criteria
    methods get_registered_repo
      importing
                objects              type cl_cts_abap_vcs_organizer_fac=>tt_object
      returning value(rs_repository) type if_cts_abap_vcs_api_facade=>ty_get_repository_response
      raising   cx_cts_abap_vcs_exception
                cx_cts_github_api_exception.

    "! Method to retreive the repository that has the package registered by using gCTS registry.
    "! The package passed as argument to the method is used to check if it has been registered in any of the repository
    "! If it is registered then the repository object is retreived and returned
    "!
    "! @parameter object     | object to be searched in the registry
    "!
    "! @parameter rs_repository     | The tadir entry of the object which matches the filter criteria
    methods get_registered_repository
      importing
                object                  type cl_cts_abap_vcs_organizer_fac=>ty_object
                recursive_package_check type abap_boolean default abap_false
                single_package_check    type abap_boolean default abap_false
      returning value(rs_repository)    type if_cts_abap_vcs_api_facade=>ty_get_repository_response
      raising   cx_cts_abap_vcs_exception
                cx_cts_github_api_exception.
    "! Method to retrieve the objects from an input object list that could be pushed to a repository
    "! This achieved by getting the objects of the repository and then comparing them with the input object list
    "!
    "! @parameter iv_repository     | repository to which the objects have to be pushed
    "! @parameter it_object_list     | the object list that has to be filtered out
    "!
    "! @parameter rt_objects_to_push     | the final object list that could be pushed to the repository
    methods get_objects_to_push
      importing
        !iv_repository            type    if_cts_abap_vcs_repository=>ty_repository_json
        !it_object_list           type tt_tadir
      returning
        value(rt_objects_to_push) type ty_objects_to_push .
    "! Method to commit and push the objects to a repository. The text passed would be used to set the commit message.
    "! If the toggle feature for asynchronous push is set, then the push objects functionality of the gCTS is used. This would mean the objects would be in an asynchronous manner.
    "! If the toggle feature for asynchronous push is not set, then the commit repository functionality with automatic push is used. This would be a synchronous opertaion.
    "!
    "! @parameter objects_to_push     | The objects that are to be committed and pushed to the remote repository
    "! @parameter commit_text     | the commit message for the push
    "!
    "! @parameter rs_response     | the response of the commit and push operation
    methods commit_and_push_to_repo
      importing objects_to_push        type tt_gcts_badi_object
                destination_repository type tt_unique_repo_object
                commit_text            type as4text
      returning value(rs_response)     type cl_cts_abap_vcs_repo_facade=>ty_commit_response.

    "! Method to check the user permission. If the user has no permissions set or has only a read permission then an error is thrown
    "!
    "! @parameter repository     | the repository to which the permission has to be checked for
    methods check_user_permission
      importing repository type if_cts_abap_vcs_repository=>ty_repository_json
      raising   cx_cts_abap_vcs_exception
                cx_cts_git_api_exception
      .
    "! Method to check if the user account is valid for the remote repository
    "!
    "! @parameter repository     | the repository to which the permission has to be checked for
    methods authentication_check
      importing repository           type if_cts_abap_vcs_repository=>ty_repository_json
      returning value(rv_user_login) type string
      raising   cx_cts_abap_vcs_exception
                cx_cts_git_api_exception.

    "! Method to check if the target system specified in the transport request and the repository vsid matches. If the
    "! value of the return is true then the target system check was successful and then further processes can be done
    "! The true value is returned even if the toggle is turned off. This would mean there is no need for a target system check.
    "!
    "! @parameter repository     | the repository for which the target has to be checked for
    "! @parameter tarsystem      | the target system in the transport request
    "! @parameter rv_system_check | the boolean return of the check
    methods target_system_check
      importing repository             type if_cts_abap_vcs_repository=>ty_repository_json
                tarsystem              type tr_target
                attributes             type trattributes optional
      returning value(rv_system_check) type boolean
      raising   cx_cts_abap_vcs_exception.

    "! Method to retrieve and log the objects that are not part of any repository after object resolvement
    "!
    "! @parameter unresolved_objects     | the repository for which the target has to be checked for
    methods log_unresolved_objects
      importing unresolved_objects type tt_gcts_badi_object.

    methods get_object_resolvement_toggle
      returning value(rv_object_resolvement_toggle) type abap_boolean.
    methods get_async_toggle
      returning value(rv_async_toggle) type abap_boolean.
    methods get_auth_toggle
      returning value(rv_auth_toggle) type abap_boolean.
    methods get_target_system_check
      returning value(rv_target_system_check) type abap_boolean.
    methods get_user_permission_toggle
      returning value(rv_user_permission_toggle) type abap_boolean.
    methods get_unique_repository
      returning value(rv_unique_repository) type abap_boolean.

    "! Class method to retrieve the parent packages of a package
    "!
    "! @parameter package        | package name for which the parent needs to retrieved
    "!
    "! @parameter r_packages     | list of packages
    class-methods get_parent_packages
      importing
                package           type devclass
      returning value(r_packages) type cl_cts_abap_vcs_organizer_fac=>tt_object.

  private section.
    data: package type string.
    data: commit_async_toggle        type boolean,
          authentication_toggle      type boolean,
          user_permission_toggle     type boolean,
          target_system_check_toggle type boolean,
          object_resolvement_toggle  type boolean,
          warning_level_toggle       type boolean,
          unique_repository          type boolean,
          badi_trigger               type boolean.
    data: object_resolvement_info  type string,
          warning_level_info       type string,
          unique_repository_info   type string,
          badi_trigger_info        type string,
          object_resolvement_level type string.
    "! Private method to analyse if a package name matches with the superpackage intialised by the constructor
    "! If the name does not match, then further check is done if the package is a subpackage of the superpackage.
    "!
    "! @parameter iv_package     | package name to be checked
    "!
    "! @parameter rv_found     | the boolean value for found or not
    methods is_package_object
      importing
        !iv_package     type devclass
      returning
        value(rv_found) type boolean .
endclass.

class ltcl_gcts_general_functions implementation.

  method constructor.
    logger ?= new cl_cts_abap_vcs_logger( section = co_general_functions ).
    me->package = package.
    me->commit_async_toggle = commit_async_toggle.
    me->authentication_toggle = authentication_toggle.
    me->target_system_check_toggle = target_system_check_toggle.
    me->user_permission_toggle = user_permission_toggle.
    me->object_resolvement_toggle = object_resolvement_toggle.
    me->object_resolvement_info = object_resolvement_info.
    me->warning_level_toggle = warning_level_toggle.
    me->unique_repository = unique_repository.
    me->badi_trigger = badi_trigger.
    me->warning_level_info = warning_level_info.
    me->unique_repository_info = unique_repository_info.
    me->badi_trigger_info = badi_trigger_info.
    me->object_resolvement_level = object_resolvement_level.
  endmethod.

  method retreive_complete_object.
    data(action) = co_retreive_complete_object.
    logger->log_info( action = action info = 'Start of Action' ).
    data: ls_tadir type tadir.
    logger->log_info( action = action info = |Calling function Module 'TR_CHECK_TYPE' for object : { object-obj_name }, { object-pgmid }, { object-object }| ).
    call function 'TR_CHECK_TYPE'
      exporting
        wi_e071  = object
      importing
        we_tadir = ls_tadir
      exceptions
        others   = 1.

    if sy-subrc = 0.
      logger->log_info( action = action info = |TADIR entry retreived : { ls_tadir-obj_name }, { ls_tadir-pgmid }, { ls_tadir-object } | ).
      rs_tadir_object-pgmid = ls_tadir-pgmid.
      rs_tadir_object-obj_type = ls_tadir-object.
      rs_tadir_object-objname = ls_tadir-obj_name.
      clear ls_tadir.
    endif.
    logger->log_info( action = action info = 'End of Action' ).
  endmethod.

  method check_object_exists.
    data(action) = co_check_object_exists.
    logger->log_info( action = action info = 'Start of Action' ).
    call function 'DEV_CHECK_OBJECT_EXISTS'
      exporting
        i_pgmid   = object-pgmid
        i_objtype = object-obj_type
        i_objname = object-objname
      importing
        e_exists  = rv_check.

    if sy-subrc = 0.
      if rv_check = abap_true.
        logger->log_info( action = action info = |object: { object-objname }, object_type : { object-obj_type }, pgmid : { object-pgmid } exists| ).
      else.
        logger->log_info( action = action info = |object: { object-objname }, object_type : { object-obj_type }, pgmid : { object-pgmid } does not exist in the abap system| ).
      endif.
    else.
      logger->log_error( action = action info = |subrc error during check_object_exists for object: { object-objname }, object_type : { object-obj_type }, pgmid : { object-pgmid }| ).
      logger->log_error( action = action info = |Setting check flag as false for the object| ).
      rv_check = abap_false.
    endif.
    logger->log_info( action = action info = 'End of Action' ).
  endmethod.

  method prepare_object.
    data: ls_tadir type tadir.
    data(action) = co_prepare_object.
    logger->log_info( action = action info = 'Start of Action' ).
    call function 'TR_TADIR_INTERFACE'
      exporting
        wi_tadir_pgmid    = object-pgmid
        wi_tadir_object   = object-obj_type
        wi_tadir_obj_name = object-objname
        wi_read_only      = 'X'
      importing
        new_tadir_entry   = ls_tadir
      exceptions
        others            = 1.
    logger->log_info( action = action info = |Read TADIR entry for { object-objname }, { object-obj_type }, { object-pgmid }| ) .
    if ls_tadir-devclass is initial.
      logger->log_info( action = action info = |No package details (devclass) in the TADIR entry. Trying to retrieve it from TADIR table| ) .
      select single devclass from tadir into ls_tadir-devclass where object = ls_tadir-object and obj_name = ls_tadir-obj_name.
      logger->log_info( action = action info = |Devclass retreived : { ls_tadir-devclass }| ) .
    endif.
    if me->object_resolvement_toggle = abap_true.
*     Registry Toggle : on, appending the objects based on the registration in the registry
      logger->log_info( action = action info = |Object Resolvement : ON| ).
      if me->object_resolvement_info = 'STATIC'.
        logger->log_info( action = action info = |Object Resolvement : ON, Strategy: Static| ).
        if me->package is initial or me->package = ''.
          logger->log_error( action = action info = 'No package to search in registry, when the registry toggle is on and registry info is PACKAGE').
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
        endif.
        if is_package_object( ls_tadir-devclass ) = abap_true and ls_tadir-genflag = abap_false.
*         The objects are appended based on the superpackage that is provided as a constant.
*         Case registry toggle on, additional info = 'PACKAGE' and the constant is set
          logger->log_info( action = action info = 'Object package matches/part of superpackage and is not a generated object').
          rs_object = ls_tadir.
        endif.
      else.
        if ls_tadir-genflag = abap_false.
          logger->log_info( action = action info = 'Object is not a generated object and is added to be pushed').
          rs_object = ls_tadir.
        endif.
      endif.
    else.
*     Registry Toggle : off, appending the objects for further processing
      logger->log_info( action = action info = |Object Resolvement Toggle : OFF| ).
      if ls_tadir-genflag = abap_false.
        logger->log_info( action = action info = 'Object is not a generated object and is added to be pushed').
        rs_object = ls_tadir.
      endif.
    endif.
    logger->log_info( action = action info = 'End of Action' ).
  endmethod.

  method is_package_object.
    if iv_package = me->package.
      rv_found = abap_true.
    else.
      call method cl_package=>load_package
        exporting
          i_package_name = iv_package
        importing
          e_package      = data(lo_package)
        exceptions
          others         = 1.
      if lo_package is not initial.
        if lo_package->super_package_name is initial.
          rv_found = abap_false.
        elseif lo_package->super_package_name = me->package .
          rv_found = abap_true.
        else.
          rv_found = is_package_object( lo_package->super_package_name  ).
        endif.
      else.
        rv_found = abap_false.
      endif.
    endif.
  endmethod.

  method get_registered_repo.
    data: repo type if_cts_abap_vcs_api_facade=>ty_get_repository_request.
    data(action) = co_get_registered_repo.
    logger->log_info( action = action info = 'Start of Action' ).
    data(lt_object) = cl_cts_abap_vcs_registry_fac=>check_objects(
              objects        = objects
              package_lookup = cl_cts_abap_vcs_registry_fac=>co_package_lookup_recursive ).
    logger->log_info( action = action info = |{ package } checked if it is registered in a repository| ).
    if line_exists( lt_object[ obj_name = package ] ).
      repo-repository = lt_object[ obj_name = package ]-repository.
      logger->log_info( action = action info = |{ package } exists in repository : { repo-repository } | ).
    endif.
    rs_repository = cl_cts_abap_vcs_api_facade=>get_repository( repo ).
    logger->log_info( action = action info = 'End of Action' ).
  endmethod.

  method get_objects_to_push.
    data: ls_single_object type if_cts_abap_vcs_repository=>ty_object.
    data(ls_objects_response) = cl_cts_abap_vcs_repo_facade=>get_objects( iv_repository-rid ).

    if ls_objects_response-objects is not initial and ls_objects_response-exception is initial.
      loop at it_object_list into data(ls_object).
        if line_exists( ls_objects_response-objects[ object = ls_object-devclass type = 'DEVC' ] ) or
            line_exists( ls_objects_response-objects[ object = me->package type = 'DEVC' ] ).
          if line_exists( ls_objects_response-objects[ object = ls_object-obj_name type = ls_object-object ] ).
            ls_single_object =  ls_objects_response-objects[ object = ls_object-obj_name type = ls_object-object ].
            append value #( object = ls_single_object-object type = ls_single_object-type ) to rt_objects_to_push.
          else.
            append value #( object = ls_object-obj_name type = ls_object-object ) to rt_objects_to_push.
          endif.
        elseif line_exists( ls_objects_response-objects[ object = ls_object-obj_name type = ls_object-object ] ).
          ls_single_object =  ls_objects_response-objects[ object = ls_object-obj_name type = ls_object-object ].
          append value #( object = ls_single_object-object type = ls_single_object-type ) to rt_objects_to_push.

        endif.
        clear ls_single_object.
      endloop.
    endif.
  endmethod.

  method commit_and_push_to_repo.
    data: ty_request              type if_cts_abap_vcs_api_facade=>ty_push_objects_request,
          lt_object_for_sync_push type if_cts_abap_vcs_transport_req=>tt_objects.
    data(action) = co_commit_and_push_to_repo.
    logger->log_info( action = action info = 'Start of Action' ).
    loop at destination_repository into data(ls_repository).
      clear ty_request.
      logger->log_info( action = action info = |Commit: { commit_text } to repository { ls_repository-rid }| ).
      ty_request-desc = commit_text.
      ty_request-repository = ls_repository-rid.
      logger->log_info( action = action info = |Retrieveing objects to push| ).
      loop at objects_to_push into data(ls_object) where associated_repo_id = ls_repository-rid .
        logger->log_info( action = action info = |Object : { ls_object-object-object }, Pgmid: { ls_object-object-pgmid }, Type: { ls_object-object-type }, User: { ls_object-object-user }| ).
        append ls_object-object to ty_request-objects.
      endloop.
      if me->commit_async_toggle = abap_true.
        logger->log_info( action = action info = 'Async toggle set. Pushing objects asynchronously' ).
        data(lv_response) = cl_cts_abap_vcs_trans_facade=>push_objects( ty_request ).
        rs_response-trkorr = lv_response-trkorr.
        rs_response-exception = lv_response-exception.
      else.
        logger->log_info( action = action info = 'Async toggle not set. Pushing objects synchronously' ).
*     Throw a warning (log) if object count is greater than 100, tabu object with * as key
        if lines( ty_request-objects ) > 100.
          logger->log_info( action = action info = 'More than 100 objects are added for Synchronous Push. This can lead to timeout' ).
        endif.
        try.
            data(system) = cl_cts_abap_vcs_system_factory=>get_instance( )->get_default_system( ).
            data(repository) = system->get_repository_by_id( id = ls_repository-rid ).
            data(auto_push) = repository->get_config_by_key( key = if_cts_abap_vcs_config_handler=>co_conf_repo_automatic_push ).
            data(auto_pull) = repository->get_config_by_key( key = if_cts_abap_vcs_config_handler=>co_conf_repo_automatic_pull ).
          catch cx_cts_abap_vcs_exception into data(exc).
        endtry.
        rs_response = cl_cts_abap_vcs_repo_facade=>commit_repository( value #(
                repository = ls_repository-rid
                message = commit_text
                objects = ty_request-objects
                auto_push = auto_push
              ) ).
      endif.
      if not rs_response-exception is initial or not rs_response-error_log is initial.
        logger->log_error( action = action info = |Error while committing the objects into repository : { ls_repository-rid }| ).
        logger->log_error( action = action info = |Exception : { rs_response-exception } | ).
        return.
      endif.
    endloop.
    logger->log_info( action = action info = 'End of Action' ).
  endmethod.

  method check_user_permission.
    if me->user_permission_toggle = abap_true.
      data(lo_system) = cl_cts_abap_vcs_system_factory=>get_instance( )->get_default_system( ).
      data(lo_repository) = lo_system->get_repository_by_id( id = repository-rid ).

      data lv_access_class type seoclsname value 'CL_CTS_ABAP_VCS_ACCESS'.
      try.
          cl_oo_class=>get_instance( lv_access_class ).
          call method (lv_access_class)=>check_permission
            exporting
              permission = 'WRITE'
              section    = 'REPOSITORY'
              repository = lo_repository->get_id( ).
        catch cx_root.
      endtry.

      if lo_repository->get_type(  ) = if_cts_abap_vcs_repository=>co_repo_type_github.
        data(api) = cl_cts_git_api=>if_cts_git_api~get_instance( api_endpoint = lo_repository->get_api_endpoint( ) ).
        data(user) = api->get_user( ).
        data(lo_remote_repository) = api->get_repository_by_name(
            name = lo_repository->get_remote_name( )
            org = lo_repository->get_remote_namespace( )
        ).
        data(lv_permission) = lo_remote_repository->get_collaborator_permission( username = user->get_login(  ) )-permission.
        if lv_permission is initial or lv_permission = 'read' or lv_permission = 'READ' or lv_permission = 'triage' or lv_permission = 'TRIAGE'.
          raise exception type cx_cts_git_api_exception.
        endif.
      endif.
    endif.
  endmethod.

  method authentication_check.
    if me->authentication_toggle = abap_true.
      data(lo_system) = cl_cts_abap_vcs_system_factory=>get_instance( )->get_default_system( ).
      data(lo_repository) = lo_system->get_repository_by_id( id = repository-rid ).
      if lo_repository->get_type(  ) = if_cts_abap_vcs_repository=>co_repo_type_github.
        data(api) = cl_cts_git_api=>if_cts_git_api~get_instance( api_endpoint = lo_repository->get_api_endpoint( ) ).
        data(user) = api->get_user( ).
        rv_user_login = user->get_login(  ).
      endif.
    endif.
  endmethod.

  method target_system_check.
    data sys_data_handler type ref to if_cts_abap_vcs_sys_data_hndlr.
    data lt_vsid_count type tt_vsid_count.
    if me->target_system_check_toggle = abap_false.
*     If the toggle is false, then there is no system check required.
      rv_system_check = abap_true.
    elseif tarsystem is initial or tarsystem = ''.
*     The target system is initial. The check is ignored in such cases since its a Local Change request
      rv_system_check = abap_true.
    elseif me->target_system_check_toggle = abap_true and me->commit_async_toggle = abap_false.
      if repository-vsid = tarsystem.
        rv_system_check = abap_true.
      else.
        rv_system_check = abap_false.
      endif.
    elseif me->target_system_check_toggle = abap_true and me->commit_async_toggle = abap_true.
      data(system) = cl_cts_abap_vcs_system_factory=>get_instance( )->get_default_system( ).
      data(repositories_data) = system->get_repositories_data( refresh = abap_true ).
*     Get a count of the virtual sids from the repository details. This count would be later
*     used to check if the repository to which the asynchronous push is being requested has a unique vsid
      loop at repositories_data into data(ls_repository).
        if lt_vsid_count is initial.
          append value #( vsid = ls_repository-vsid count = 1 ) to lt_vsid_count.
        else.
          if line_exists( lt_vsid_count[ vsid = ls_repository-vsid ] ).
            data: ls_new_vsid_count type ty_vsid_count.
            clear ls_new_vsid_count.
            data(new_count) = lt_vsid_count[ vsid = ls_repository-vsid ]-count + 1.
            ls_new_vsid_count-vsid = ls_repository-vsid.
            ls_new_vsid_count-count = new_count.
            modify lt_vsid_count from ls_new_vsid_count transporting count where vsid = ls_repository-vsid.
          else.
            append value #( vsid = ls_repository-vsid count = 1 ) to lt_vsid_count.
          endif.
        endif.
      endloop.
*     Check if the repository vsid is unique. If it is unique then the target system check returns true
      if lt_vsid_count[ vsid = repository-vsid ]-count = 1.
        rv_system_check = abap_true.
      else.
*      Advanced check if the vsid is not unique for asynchronous push, check if attribute SAP_SCTS_AVCS_TARGET is set in Tr
*      and if VCS_RESTRICTED_PUSH is true.
        if line_exists( repository-config[ key = if_cts_abap_vcs_config_handler=>co_conf_repo_vcs_restrict_push ] ).
*         Retrieve VCS_RESTRICTED_PUSH for the repository
          data(restricted_push) = repository-config[ key = if_cts_abap_vcs_config_handler=>co_conf_repo_vcs_restrict_push ].
        endif.
        if line_exists( attributes[ attribute = if_cts_abap_vcs_transport_req=>co_tr_repo_attribute  ] ) and restricted_push-value = 'X'.
          rv_system_check = abap_true.
        else.
          rv_system_check = abap_false.
        endif.
      endif.
    endif.
  endmethod.

  method prepare_objects_for_push.
    data: local_object type ty_gcts_badi_object,
          object       type ty_repository_objects.
    data: ls_tr_key     type e071k,
          ls_wbo_object type  cl_cts_abap_vcs_organizer_fac=>ty_object.
    data(action) = co_prepare_objects_for_push.
    loop at tr_objects into data(ls_tr_objects).
      clear local_object.
      if ls_tr_objects-object <> 'CDAT' and ls_tr_objects-object <> 'TABU' and ls_tr_objects-object <> 'VDAT' and ls_tr_objects-object <> 'TDAT'.
        logger->log_info( action = action info = |Object pgmid: { ls_tr_objects-pgmid }, name: { ls_tr_objects-obj_name } type: { ls_tr_objects-object }| ).
*     Loop through all the non customizing objects and store them for further processing
        local_object-is_customizing = abap_false.
        object-objname = ls_tr_objects-obj_name.
        object-obj_type = ls_tr_objects-object.
        object-pgmid = ls_tr_objects-pgmid.
        if ls_tr_objects-pgmid = 'LIMU'.
          data(full_object) = me->retreive_complete_object( ls_tr_objects ).
          if full_object is initial.
*          If full object could not be retrieved the object would not be added to the return table
            continue.
          else.
            object = full_object.
          endif.
        endif.
        data(check_object_exists) = me->check_object_exists( object ).
        if check_object_exists = abap_true.
          data(ls_tadir) = me->prepare_object( object ).
          if not line_exists( rt_object_list[ object-object = ls_tadir-obj_name ] ).
            logger->log_info( action = action info = |Appended Tadir: { ls_tadir-pgmid }, { ls_tadir-obj_name }, { ls_tadir-object } to object list| ).
*           Append object wbo for further checks
            ls_wbo_object-obj_name = ls_tadir-obj_name.
            ls_wbo_object-pgmid = ls_tadir-pgmid.
            ls_wbo_object-object = ls_tadir-object.
            ls_wbo_object-devclass = ls_tadir-devclass.
            local_object-object_for_wbo = ls_wbo_object.
            clear ls_wbo_object.
*           Append object for transport request
            local_object-object-object = ls_tadir-obj_name.
            local_object-object-pgmid = ls_tadir-pgmid.
            local_object-object-type = ls_tadir-object.
            local_object-dev_class = ls_tadir-devclass.
            local_object-gen_flag = ls_tadir-genflag.
            append local_object to rt_object_list.
            clear local_object.
          endif.
        else.
*           Handle Exception Here
          logger->log_error( action = action info = 'Object does not exist in system').
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
        endif.
      endif.
    endloop.

    loop at tr_keys into ls_tr_key.
      data: lt_objects_to_convert type cl_cts_abap_vcs_organizer_fac=>tt_object.
*     Loop through all the customizing object
      logger->log_info( action = action info = |Customizing Object pgmid: { ls_tr_key-pgmid }, name: { ls_tr_key-objname } type: { ls_tr_key-object }| ).
      clear local_object.
      local_object-is_customizing = abap_true.

      clear ls_wbo_object.
      ls_wbo_object-obj_name = ls_tr_key-mastername.
      ls_wbo_object-pgmid = ls_tr_key-pgmid.
      ls_wbo_object-object = ls_tr_key-mastertype.
      append ls_tr_key to ls_wbo_object-e071k.

      if ls_wbo_object is not initial.
        local_object-object_for_wbo = ls_wbo_object.
      endif.

      append ls_wbo_object to lt_objects_to_convert.
      data(lt_objects_converted) = cl_cts_abap_vcs_registry=>convert_objects_to_registry( objects = lt_objects_to_convert ) .

      if line_exists( lt_objects_converted[ obj_name = ls_wbo_object-obj_name ] ).
        local_object-object-object = lt_objects_converted[ obj_name = ls_wbo_object-obj_name ]-obj_name.
        local_object-object-pgmid = lt_objects_converted[ obj_name = ls_wbo_object-obj_name ]-pgmid.
        local_object-object-type = lt_objects_converted[ obj_name = ls_wbo_object-obj_name ]-object.
        local_object-object-keys = lt_objects_converted[ obj_name = ls_wbo_object-obj_name ]-keys.
      endif.

      append local_object to rt_object_list.
    endloop.

  endmethod.

  method get_async_toggle.
    rv_async_toggle = me->commit_async_toggle.
  endmethod.

  method get_auth_toggle.
    rv_auth_toggle = me->authentication_toggle.
  endmethod.

  method get_object_resolvement_toggle.
    rv_object_resolvement_toggle = me->object_resolvement_toggle.
  endmethod.

  method get_target_system_check.
    rv_target_system_check = me->target_system_check_toggle.
  endmethod.

  method get_user_permission_toggle.
    rv_user_permission_toggle = me->user_permission_toggle.
  endmethod.

  method set_repository_for_objects.
    data(action) = co_set_repository_for_objects.
    data(system) = cl_cts_abap_vcs_system_factory=>get_instance( )->get_default_system( ).
*    data: ls_object_resolvement_info type ty_object_resolvement_info.

*    /ui2/cl_json=>deserialize(
*           exporting
*             json = me->object_resolvement_info
*             pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*           changing
*             data = ls_object_resolvement_info
*         ).
    if me->object_resolvement_toggle = abap_true and me->object_resolvement_info = 'STATIC'.
**********************************************************************
*     Resolution Strategy : STATIC
**********************************************************************
*     When the Object resolution toggle is on and the Object resolution toggle information has STATIC
*     Then the constant value for package is retrieved and then the repository to which this package is registered is checked
*     The repository information is then passed back to the badi.
      logger->log_info( action = action info = 'Resolvement Toggle on, and strategy is static' ).
      if me->package is initial or me->package = ''.
        logger->log_error( action = action info = 'No package to search in registry, when the Resolvement Toggle is on and strategy is static').
        raise exception type cx_cts_abap_vcs_exception
          exporting
            textid = cx_cts_abap_vcs_exception=>repository_not_found.
      endif.
      try.
          data: lt_object type cl_cts_abap_vcs_organizer_fac=>tt_object .
          append value #( pgmid    = 'R3TR' object   = 'DEVC' obj_name = me->package  ) to lt_object.
          data(ls_repository) = me->get_registered_repo( lt_object ).
        catch cx_cts_github_api_exception into data(git_exc).
          logger->log_error( action = action info = 'The resolution strategy: STATIC failed').
          logger->log_error( action = action info = |{ git_exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
        catch cx_cts_abap_vcs_exception into data(exc).
          logger->log_error( action = action info = 'The resolution strategy: STATIC failed').
          logger->log_error( action = action info = |{ exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
      endtry.
      loop at object_list into data(ls_object).
*       Assign the repository found from the registry to the return object list.
*       Since this is a registry, super package based operation, every object would have the same repository
        ls_object-associated_repo = ls_repository-result.
        ls_object-associated_repo_id = ls_repository-result-rid.
        append ls_object to set_repository_response-resolved_objects.
      endloop.
    elseif me->object_resolvement_toggle = abap_true and me->object_resolvement_info = 'SINGLE'.
**********************************************************************
*     Resolution Strategy : Single
**********************************************************************
*     Check if object exists in the registry
*     If it exists then the repository is retreived and then the information is returned
      logger->log_info( action = action info = 'Resolvement Toggle on, and strategy is single' ).
      try.
          loop at object_list into ls_object.
            ls_repository = me->get_registered_repository( ls_object-object_for_wbo ).
            if ls_repository-result is not initial and ls_repository-exception is initial.
              ls_object-associated_repo = ls_repository-result.
              ls_object-associated_repo_id = ls_repository-result-rid.
              append ls_object to set_repository_response-resolved_objects.
            else.
              append ls_object to set_repository_response-unresolved_objects.
            endif.
          endloop.
        catch cx_cts_github_api_exception into git_exc.
          logger->log_error( action = action info = 'The resolution strategy: SINGLE failed').
          logger->log_error( action = action info = |{ git_exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
        catch cx_cts_abap_vcs_exception into exc.
          logger->log_error( action = action info = 'The resolution strategy: SINGLE failed').
          logger->log_error( action = action info = |{ exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
      endtry.
    elseif me->object_resolvement_toggle = abap_true and me->object_resolvement_info = 'PACKAGE'.
**********************************************************************
*     Resolution Strategy : PACKAGE
**********************************************************************
*     Check if the objects exists in the registry
*     Check if the object or the package it exists in is registered in any repository
      logger->log_info( action = action info = 'Resolvement Toggle on, and strategy is package' ).
      try.
          loop at object_list into ls_object.
            ls_repository = me->get_registered_repository( object = ls_object-object_for_wbo single_package_check = abap_true ).
            if ls_repository-result is not initial and ls_repository-exception is initial..
              ls_object-associated_repo = ls_repository-result.
              ls_object-associated_repo_id = ls_repository-result-rid.
              append ls_object to set_repository_response-resolved_objects.
            else.
              append ls_object to set_repository_response-unresolved_objects.
            endif.
          endloop.
        catch cx_cts_github_api_exception into git_exc.
          logger->log_error( action = action info = 'The resolution strategy: PACKAGE failed').
          logger->log_error( action = action info = |{ git_exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
        catch cx_cts_abap_vcs_exception into exc.
          logger->log_error( action = action info = 'The resolution strategy: PACKAGE failed').
          logger->log_error( action = action info = |{ exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
      endtry.
    elseif me->object_resolvement_toggle = abap_true and  me->object_resolvement_info = 'RECURSIVE_PACKAGE'.
**********************************************************************
*     Resolution Strategy : RECURSIVE PACKAGE
**********************************************************************
*     Check if the object or all the packages until the top exists registered in a repository
      logger->log_info( action = action info = 'Resolvement Toggle on, and strategy is recursive package' ).
      try.
          loop at object_list into ls_object.
            ls_repository = me->get_registered_repository( object = ls_object-object_for_wbo recursive_package_check = abap_true ).
            if ls_repository-result is not initial and ls_repository-exception is initial..
              ls_object-associated_repo = ls_repository-result.
              ls_object-associated_repo_id = ls_repository-result-rid.
              append ls_object to set_repository_response-resolved_objects.
            else.
              append ls_object to set_repository_response-unresolved_objects.
            endif.
          endloop.
        catch cx_cts_github_api_exception into git_exc.
          logger->log_error( action = action info = 'The resolution strategy: RECURSIVE PACKAGE failed').
          logger->log_error( action = action info = |{ git_exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
        catch cx_cts_abap_vcs_exception into exc.
          logger->log_error( action = action info = 'The resolution strategy: RECURSIVE PACKAGE failed').
          logger->log_error( action = action info = |{ exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
      endtry.
    elseif me->object_resolvement_toggle = abap_true and  me->object_resolvement_info = 'FULL'.
**********************************************************************
*     Resolution Strategy : FULL
**********************************************************************
*     Check if the objects exists in the registry and if it does
*     does not exist then retrieve the object list of all the repositories
*
      logger->log_info( action = action info = 'Resolvement Toggle on, and strategy is full package' ).
      logger->log_info( action = action info = 'This is a very resource intensive process, avoid this strategy if possible').
      try.
          data(lt_available_repositories) = system->get_repositories( refresh = abap_true ).
          loop at object_list into ls_object.
            data(is_resolved) = abap_false.
            ls_repository = me->get_registered_repository( object = ls_object-object_for_wbo recursive_package_check = abap_true ).
            if ls_repository-result is not initial and ls_repository-exception is initial.
              ls_object-associated_repo = ls_repository-result.
              ls_object-associated_repo_id = ls_repository-result-rid.
              append ls_object to set_repository_response-resolved_objects.
              is_resolved = abap_true.
            else.
              data: lt_object_with_packages type standard table of cl_cts_abap_vcs_organizer_fac=>ty_object .
              clear lt_object_with_packages.
              if not lines( ls_object-object_for_wbo-e071k ) > 0.
*               retrieve all the packages and parent packages of the object if not customizing. Only Workbench objects has packages
                append ls_object-object_for_wbo to lt_object_with_packages.
                if ls_object-dev_class is not initial.
                  data(ls_package_heirarchy) = get_parent_packages( package = ls_object-dev_class ).
                  append lines of ls_package_heirarchy to lt_object_with_packages.
                endif.
              endif.
              loop at lt_available_repositories into data(ls_available_repository).
                if is_resolved = abap_true.
                  exit.
                endif.
                if line_exists( repository_cache_table[ repository_id = ls_available_repository-id ] ).
                  data(repository_objects) = repository_cache_table[ repository_id = ls_available_repository-id ]-repository_objects.
                  data(repository_json) = repository_cache_table[ repository_id = ls_available_repository-id ]-repository_json.
                else.
                  data(repository) = system->get_repository_by_id( ls_available_repository-id ).
                  repository_objects = repository->get_objects(  ).
                  repository_json = repository->to_json_object(  ).
                endif.
                if lines( lt_object_with_packages ) > 0.
*                Process Workbench Objects
                  loop at lt_object_with_packages into data(ls_object_with_packages).
                    if line_exists( repository_objects[ object = ls_object_with_packages-obj_name pgmid = ls_object_with_packages-pgmid type = ls_object_with_packages-object ] ).
                      ls_object-associated_repo = repository_json.
                      ls_object-associated_repo_id = repository_json-rid.
                      append ls_object to set_repository_response-resolved_objects.
                      is_resolved = abap_true.
                      exit.
                    endif.
                  endloop.
                else.
*                Process Customizing Objects
                  if line_exists( repository_objects[ object = ls_object_with_packages-obj_name pgmid = ls_object_with_packages-pgmid type = ls_object_with_packages-object ] ).
                    data: lt_repo_object_e071k  type if_cts_abap_vcs_client_handler=>tt_e071k .
                    loop at repository_objects into data(ls_repository_object) where object = ls_object_with_packages-obj_name and pgmid = ls_object_with_packages-pgmid and type = ls_object_with_packages-object.
                      append lines of ls_repository_object-e071k to lt_repo_object_e071k.
                    endloop.
                    loop at ls_object-object_for_wbo-e071k into data(ls_e071k).
                      if line_exists( lt_repo_object_e071k[ objname = ls_e071k-objname mastertype = ls_e071k-mastertype mastername = ls_e071k-mastername tabkey = ls_e071k-tabkey ] ).
                        is_resolved = abap_true.
                      else.
                        is_resolved = abap_false.
                        clear lt_repo_object_e071k.
                        exit.
                      endif.
                    endloop.
                    clear lt_repo_object_e071k.
                    if is_resolved = abap_true.
                      ls_object-associated_repo = repository_json.
                      ls_object-associated_repo_id = repository_json-rid.
                      append ls_object to set_repository_response-resolved_objects.
                      exit.
                    endif.
                  endif.
                endif.
              endloop.
            endif.
            if is_resolved = abap_false.
              append ls_object to set_repository_response-unresolved_objects.
            endif.
          endloop.
        catch cx_cts_github_api_exception into git_exc.
          logger->log_error( action = action info = 'The resolution strategy: FULL failed').
          logger->log_error( action = action info = |{ git_exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
        catch cx_cts_abap_vcs_exception into exc.
          logger->log_error( action = action info = 'The resolution strategy: FULL failed').
          logger->log_error( action = action info = |{ exc->get_message( ) }| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              textid = cx_cts_abap_vcs_exception=>repository_not_found.
      endtry.
    else.
**********************************************************************
*     Resolution Strategy : OFF
**********************************************************************
*     Task based comitting would be the default in this case
*
      data(repositories_data) = system->get_repositories_data( refresh = abap_true ).
      if line_exists( repositories_data[ vsid = vsid ] ).
        data(ls_repository_raw) = repositories_data[ vsid = vsid ].
        data(ls_repository2) = cl_cts_abap_vcs_api_facade=>get_repository( request = value #( repository = ls_repository_raw-rid ) ).
        loop at object_list into ls_object.
          ls_object-associated_repo = ls_repository2-result.
          ls_object-associated_repo_id = ls_repository2-result-rid.
          append ls_object to set_repository_response-resolved_objects.
        endloop.
      endif.

    endif.

  endmethod.

  method get_registered_repository.
    data: repo    type if_cts_abap_vcs_api_facade=>ty_get_repository_request,
          objects type cl_cts_abap_vcs_organizer_fac=>tt_object.
    data(action) = co_get_registered_repo.
    logger->log_info( action = action info = 'Start of Action' ).
    logger->log_info( action = action info = |Searching for object : { object-obj_name }| ).
    append object to objects.
    if recursive_package_check = abap_true.
      data(lt_object) = cl_cts_abap_vcs_registry_fac=>check_objects(
          objects        = objects
          package_lookup = cl_cts_abap_vcs_registry_fac=>co_package_lookup_recursive ).
    elseif single_package_check = abap_true.
      lt_object = cl_cts_abap_vcs_registry_fac=>check_objects(
         objects        = objects
         package_lookup = cl_cts_abap_vcs_registry_fac=>co_package_lookup_single ).
    else.
      lt_object = cl_cts_abap_vcs_registry_fac=>check_objects(
              objects        = objects ).
    endif.

    if line_exists( lt_object[ obj_name = object-obj_name ] ).
      repo-repository = lt_object[ obj_name = object-obj_name ]-repository.
      logger->log_info( action = action info = |{ object-obj_name } exists in repository : { repo-repository } | ).
    endif.
    rs_repository = cl_cts_abap_vcs_api_facade=>get_repository( repo ).
    logger->log_info( action = action info = 'End of Action' ).
  endmethod.

  method retreive_unique_repositories.
    data: tt_unique_repo_ids    type standard table of scts_abap_vcs_repository_id.
    data(action) = co_retrieve_unique_repos.
    logger->log_info( action = action info = 'Start of Action' ).
    tt_unique_repo_ids = value #(
    for groups value of ls_badi_object in object_list
    group by ls_badi_object-associated_repo_id without members ( value ) ).
    loop at tt_unique_repo_ids into data(lv_repository).
      if line_exists( object_list[ associated_repo_id = lv_repository ] ).
        append object_list[ associated_repo_id = lv_repository ]-associated_repo to rt_unique_repositories.
      endif.
    endloop.
    logger->log_info( action = action info = 'End of Action' ).
  endmethod.

  method get_unique_repository.
    rv_unique_repository = me->unique_repository.
  endmethod.

  method log_unresolved_objects.
    data(action) = co_log_unresolved_obects.
    logger->log_start( action = action ).
    logger->log_info( action = action info = |Relation identifier log level : { me->object_resolvement_level } | ).
    loop at unresolved_objects into data(ls_unresolved_objects).
      if lines( ls_unresolved_objects-object_for_wbo-e071k ) > 0.
*    log customizing objects
        logger->log_info( action = action info = |Pgmid: { ls_unresolved_objects-object_for_wbo-pgmid }, Object: { ls_unresolved_objects-object_for_wbo-object }, Object Name : { ls_unresolved_objects-object_for_wbo-obj_name } | ).
        loop at ls_unresolved_objects-object-keys into data(ls_key).
          logger->log_info( action = action info = |Table Name: { ls_key-tabname } | ).
          loop at ls_key-columns into data(ls_column).
            logger->log_info( action = action info = |Key: { ls_column-key } Value: { ls_column-value } | ).
          endloop.
        endloop.
      else.
*    Log work bench objects
        logger->log_info( action = action info = |Pgmid: { ls_unresolved_objects-object_for_wbo-pgmid }, Object: { ls_unresolved_objects-object_for_wbo-object }, Object Name : { ls_unresolved_objects-object_for_wbo-obj_name } | ).
      endif.
    endloop.
    logger->log_end( action = action ).
  endmethod.

  method get_parent_packages.
    cl_package=>load_package(
             exporting
               i_package_name = package
             importing
               e_package      = data(lo_package)
             exceptions
               others         = 1 ).
    if sy-subrc = 0 and not lo_package is initial and lo_package is bound.
      append value #( pgmid = if_cts_vcs_file_handler_entity=>co_object_pgmid object = if_cts_vcs_file_handler_entity=>co_object_devc obj_name = lo_package->package_name  ) to r_packages.
      if not lo_package->super_package_name is initial.
        append lines of get_parent_packages( package = lo_package->super_package_name ) to r_packages.
      endif.
    endif.
  endmethod.

endclass.

class ltcl_gcts_badi_trigger definition.
  public section.
*   Constants for the scope for the badi trigger
    constants co_scope_user type string value 'USER'.
    constants co_scope_objects type string value 'OBJECTS'.
    constants co_scope_tr type string value 'TRANSPORT'.
*   Constants for the logger class.
    constants co_badi_trigger type string value 'BADI_TRIGGER_CLASS'.
    constants co_check_badi_trigger type string value 'CHECK_OTHER_BADI_TRIGGER'.
    constants co_check_user_trigger type string value 'CHECK_USER_TRIGGER'.
    data:      logger  type ref to if_cts_abap_vcs_logger.

    types tt_users type standard table of string with default key.

    types:
      begin of ty_badi_trigger,
        scope             type string,
        user              type tt_users,
        table             type string,
        registry          type boolean,
        local_object_list type boolean,
      end of ty_badi_trigger .

    methods constructor
      importing badi_trigger_toggle type boolean
                badi_trigger_info   type string
                target_system       type tr_target.
    methods check_other_badi_triggers
      returning value(rs_is_okay) type boolean.
    methods check_user_trigger
      returning value(rs_is_okay) type boolean.
    methods set_object_list
      importing it_object_list type ltcl_gcts_general_functions=>tt_gcts_badi_object.
  private section.
    data: badi_trigger_toggle type boolean,
          badi_trigger_info   type string,
          target_system       type tr_target,
          object_list         type  ltcl_gcts_general_functions=>tt_gcts_badi_object.
endclass.

class ltcl_gcts_badi_trigger implementation.

  method check_other_badi_triggers.
    data(action) = co_check_badi_trigger.
    data: ls_badi_trigger_req type ty_badi_trigger.
    if me->badi_trigger_toggle = abap_false.
*     Check if the badi trigger is set or not. If it is not set, then immediately return a false flag
*     This would indicate that the badi cannot be continued.
      logger->log_info( action = action info = |BAdI trigger is not set. Gcts badi would not continue execution| ).
      rs_is_okay = abap_false.
      return.
    endif.
    /ui2/cl_json=>deserialize(
       exporting
         json = badi_trigger_info
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
       changing
         data = ls_badi_trigger_req
     ).
    logger->log_info( action = action info = |BAdI Trigger with scope : { ls_badi_trigger_req-scope }| ).
    if ls_badi_trigger_req-scope = co_scope_user.
***************************************************************************
*   The user check has already passed and because of this it can be skipped
***************************************************************************
      rs_is_okay = abap_true.
      return.
    elseif ls_badi_trigger_req-scope = co_scope_tr.
****************************************************
*   if the scope is 'TRANSPORT'
*   The badi would run for every target system that is a valid VSID. This would mean for all systems which has the tms parameter 'NON_ABAP_SYSTEM' and value as 'VCS'.
****************************************************
      logger->log_info( action = action info = |BAdI Trigger : target system { me->target_system }| ).
      data(create_parameter_object) = cl_cts_tms_tp_parameter_config=>create( ).
      try.
          data(gcts_param) = create_parameter_object->if_cts_tms_tpprofile_parameter~get_parameter_value( sysname = me->target_system name = if_cts_abap_vcs_system=>co_tms_vcs_system ).
          if gcts_param = if_cts_abap_vcs_system=>co_tms_vcs_system_value.
            logger->log_info( action = action info = |'NON_ABAP_SYSTEM' TMS parameter is VCS. GCTS BAdI triggered | ).
            rs_is_okay = abap_true.
            return.
          else.
            logger->log_info( action = action info = |'NON_ABAP_SYSTEM' TMS parameter is not VCS. GCTS BAdI cannot be triggered | ).
            rs_is_okay = abap_false.
            return.
          endif.
        catch cx_cts_tms_config_exception.
          logger->log_error( action = action info = |Error while retrieving 'NON_ABAP_SYSTEM' TMS parameter. GCTS BAdI cannot be triggered | ).
          rs_is_okay = abap_false.
          return.
      endtry.
    elseif ls_badi_trigger_req-scope = co_scope_objects.
**********************************************************
*   if the scope is 'OBJECTS'
*   if the registry field is set as true, then the registry is checked if the object existing in the TR are already registered then the badi is continued
*   if the local_object_list is set to true then, the local object list of all the repositories are also checked. This is a very expensive call
*   Use this only in case the other two scopes does not fit the need
**********************************************************
      logger->log_info( action = action info = |BAdI Trigger : registry: { ls_badi_trigger_req-registry }, Local Object List : { ls_badi_trigger_req-local_object_list  }| ).
      loop at me->object_list into data(ls_object).
        if ls_badi_trigger_req-registry = abap_true.
*        Check if the objects in the TR exists in the registry.
          data: objects type cl_cts_abap_vcs_organizer_fac=>tt_object.
          logger->log_info( action = action info = 'Start of Action' ).
          logger->log_info( action = action info = |Searching for object : { ls_object-object_for_wbo-obj_name }| ).
          append ls_object-object_for_wbo to objects.
          try.
              data(rv_checked_object) = cl_cts_abap_vcs_registry_fac=>check_objects(
                objects        = objects
                package_lookup = cl_cts_abap_vcs_registry_fac=>co_package_lookup_recursive ).
              loop at rv_checked_object into data(ls_checked_object).
                if ls_checked_object-repository is not initial and ls_checked_object-repository <> ''.
                  rs_is_okay = abap_true.
                  return.
                endif.
              endloop.
            catch cx_cts_abap_vcs_exception.
            catch cx_cts_github_api_exception.
          endtry.
        endif.
        if ls_badi_trigger_req-local_object_list = abap_true.
*       Check if the objects in the TR exists in the local object list.
*       This would be very expensive since every repository object list needs to be retrieved and the looped through to identify
          logger->log_info( action = action info = |BAdI Trigger for local object list has been set| ).
          try.
              data(system) = cl_cts_abap_vcs_system_factory=>get_instance( )->get_default_system( ).
              data(lt_available_repositories) = system->get_repositories( refresh = abap_true ).
              clear ltcl_gcts_general_functions=>repository_cache_table.
              loop at lt_available_repositories into data(ls_available_repisitory).
                data(repository) = system->get_repository_by_id( ls_available_repisitory-id ).
                data(repository_objects) = repository->get_objects(  ).
                data(ls_repository_json) = repository->to_json_object(  ).
*               Set cache for repository objects for avoid retrieval again
                append value #( repository_json = ls_repository_json
                                repository_objects = repository_objects
                                repository_id = ls_available_repisitory-id ) to ltcl_gcts_general_functions=>repository_cache_table.
                if line_exists( repository_objects[ object = ls_object-object_for_wbo-obj_name pgmid = ls_object-object_for_wbo-pgmid type = ls_object-object_for_wbo-object ] ).
                  logger->log_info( action = action info = |Object { ls_object-object_for_wbo-obj_name },{ ls_object-object_for_wbo-object } exists in repository { ls_available_repisitory-id } | ).
                  logger->log_info( action = action info = |gCTS BAdI trigger check successful for full local object list | ).
                  rs_is_okay = abap_true.
                  return.
                elseif ls_object-dev_class is not initial.
*                If the object doesn't exists check for its package and the super packages as well
                  data(lt_package_heirarchy) = ltcl_gcts_general_functions=>get_parent_packages( package = ls_object-dev_class ).
                  loop at lt_package_heirarchy into data(ls_super_package).
                    if line_exists( repository_objects[ object = ls_super_package-obj_name pgmid = ls_super_package-pgmid type = ls_super_package-object ] ).
                      logger->log_info( action = action info = |Object's super package { ls_super_package-obj_name } exists in repository { ls_available_repisitory-id } | ).
                      logger->log_info( action = action info = |gCTS BAdI trigger check successful for full local object list | ).
                      rs_is_okay = abap_true.
                      return.
                    endif.
                  endloop.
                endif.
              endloop.
            catch cx_cts_abap_vcs_exception into data(exc).
              logger->log_error( action = action info = |Error while BAdI Trigger check for local objects| ).
              logger->log_error( action = action info = |Error Message : { exc->get_message(  ) } | ).
          endtry.
        endif.
      endloop.
    else.
*      The scope defined for the badi trigger is wrong. The trigger cannot be set.
      logger->log_error( action = action info = 'The scope provided in the badi trigger toggle is not a valid value' ).
      rs_is_okay = abap_false.
      return.
    endif.
*   Default return is false. All the true cases immediately returns for further processing.
    rs_is_okay = abap_false.
  endmethod.

  method constructor.
    logger ?= new cl_cts_abap_vcs_logger( section = co_badi_trigger ).
    me->badi_trigger_toggle = badi_trigger_toggle.
    me->badi_trigger_info = badi_trigger_info.
    me->target_system = target_system.
  endmethod.

  method set_object_list.
    me->object_list = it_object_list.
  endmethod.

  method check_user_trigger.
    data(action) = co_check_user_trigger.
    data: ls_badi_trigger_req type ty_badi_trigger.
    if me->badi_trigger_toggle = abap_false.
*     Check if the badi trigger is set or not. If it is not set, then immediately return a false flag
*     This would indicate that the badi cannot be continued.
      logger->log_info( action = action info = |BAdI trigger is not set. Gcts badi would not continue execution| ).
      rs_is_okay = abap_false.
      return.
    endif.
    /ui2/cl_json=>deserialize(
       exporting
         json = badi_trigger_info
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
       changing
         data = ls_badi_trigger_req
     ).
    if ls_badi_trigger_req-scope = co_scope_user.
      logger->log_info( action = action info = |BAdI Trigger with user scope| ).
****************************************************
*   if the scope is 'USER', then it would take in consideration the fields for user and table, the user field can be single value or an array of values.
*   The table could be provided with a table name which has in it a column field 'bname'.
*   The usernames in the table and the user field would be combined together to check if the user who is executing the badi is a valid user to trigger it.
****************************************************
      logger->log_info( action = action info = |BAdI Trigger : information { badi_trigger_info }| ).
      data: lt_usernames type standard table of string.
      if ls_badi_trigger_req-user is not initial or lines( ls_badi_trigger_req-user ) > 0 .
        loop at ls_badi_trigger_req-user into data(ls_user).
          append ls_user to lt_usernames.
        endloop.
      endif.
      if ls_badi_trigger_req-table is not initial or ls_badi_trigger_req-table  <> ''.
        data: lt_usernames_from_table type table of xubname with default key,
              lt_dd02l                type table of dd02l.
        select * from dd02l where tabname = @ls_badi_trigger_req-table into table @lt_dd02l.
        if sy-subrc = 0.
          select bname from (ls_badi_trigger_req-table) into table @lt_usernames_from_table .
          if sy-subrc = 0.
            loop at lt_usernames_from_table into data(ls_usernames_from_table).
              append ls_usernames_from_table to lt_usernames.
            endloop.
          endif.
        endif.
      endif.

      if lt_usernames is not initial.
        loop at lt_usernames into data(ls_username).
          if to_upper( ls_username ) = sy-uname.
            logger->log_info( action = action info = |User { sy-uname } found in the approved users who can run the gcts BAdI| ).
            rs_is_okay = abap_true.
            return.
          endif.
        endloop.
      endif.
    else.
*     Continue since the scope is not user check.
      rs_is_okay = abap_true.
    endif.
  endmethod.

endclass.

class ltcl_gcts_repository_eval definition.
  public section.
*   Constant for logging the registry functions class.
    constants co_repository_eval type string value 'GIT_FORWARD_BADI_REPOSITORY_EVALUATION'.
    constants co_repository_eval_contr type string value 'REPOSITORY_EVALUATION_CONTROLLER'.
    constants co_repository_eval_errors type string value 'CHECKING_ERRORS_IN_LOGS'.
    types:
      begin of ty_toggle_info,
        toggle_name  type string,
        toggle_value type boolean,
      end of ty_toggle_info .

    types tt_toggle_info type standard table of ty_toggle_info with default key.
    data:      logger  type ref to if_cts_abap_vcs_logger.
    methods constructor
      importing tech_status_check    type boolean
                changes_pulled_check type boolean
                check_vsid           type boolean
                object_list_eval     type boolean
                async_commit         type boolean.

    methods repository_eval_controller
      importing repository             type ref to if_cts_abap_vcs_repository
                target_system          type tr_target
                tr_number              type trkorr
      returning value(is_eval_success) type boolean
      raising   cx_cts_abap_vcs_exception.

    methods check_error_in_logs
      importing operation_constant   type string optional
      returning value(errors_exists) type boolean.

    class-methods evaluation_toggle_information
      importing info           type string
      returning value(toggles) type tt_toggle_info.

  private section.
    data:
      tech_status_check    type boolean,
      changes_pulled_check type boolean,
      check_vsid           type boolean,
      object_list_eval     type boolean,
*      repository           type ref to cl_cts_abap_vcs_repository,
*      target_system        type tr_target,
      async_commit         type boolean.
endclass.

class ltcl_gcts_repository_eval implementation.
  method constructor.
    logger ?= new cl_cts_abap_vcs_logger( section = co_repository_eval ).
    me->tech_status_check = tech_status_check.
    me->changes_pulled_check = changes_pulled_check.
    me->check_vsid = check_vsid.
    me->object_list_eval = object_list_eval.
    me->async_commit = async_commit.
  endmethod.
  method repository_eval_controller.
    data(action) = co_repository_eval_contr.
    logger->log_start( action = action info = 'Start of Action' ).

**************************************
*     Check repository status toggle, this includes technical status, and if the latest changes were pulled
*     Check technical status toggle
*     Check no conflicts toggle
**************************************
    if me->tech_status_check = abap_true.
      logger->log_info( action = action info = 'Checking Repository status' ).
      data(repo_status) = repository->get_status( ).
      if not repo_status eq if_cts_abap_vcs_repository=>co_repository_status_ready.
        logger->log_error( action = action info = |Repository status is : { repo_status }| ).
        logger->log_error( action = action info = 'Repository Status check failed' ).
        raise exception type cx_cts_abap_vcs_exception
          exporting
            message_variable_1 = zcl_im_gcts_badi=>co_repo_status_fail.
      endif.
      logger->log_info( action = action info = 'Checking Technical status' ).
      try.
          data(technical_status) = repository->get_technical_status( ).
          if technical_status-branch_tracking_status-behind_count is not initial and technical_status-branch_tracking_status-behind_count > 0.
            logger->log_error( action = action info = |Behind count for repository : { technical_status-branch_tracking_status-behind_count }| ).
            logger->log_error( action = action info = |Status check failed| ).
            raise exception type cx_cts_abap_vcs_exception
              exporting
                message_variable_1 = zcl_im_gcts_badi=>co_status_fail.
          elseif technical_status-state-name <> 'SAFE'.
            logger->log_error( action = action info = |State of the repository is : { technical_status-state-name } and not 'SAFE' | ).
            logger->log_error( action = action info = |Status check failed| ).
            raise exception type cx_cts_abap_vcs_exception
              exporting
                message_variable_1 = zcl_im_gcts_badi=>co_status_fail.
          endif.
*          "doesn't work properly if registry was used for object relation detection
*          if check_error_in_logs( 'TECHNICAL_STATUS' ) = abap_true.
*            logger->log_error( action = action info = |Errors exists in logs for Status check| ).
*            raise exception type cx_cts_abap_vcs_exception
*              exporting
*                message_variable_1 = zcl_im_gcts_badi=>co_status_fail.
*          endif.
        catch cx_cts_abap_vcs_exception.
          logger->log_error( action = action info = |Error while retrieving status of repository | ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              message_variable_1 = zcl_im_gcts_badi=>co_status_fail.
      endtry.
      logger->log_info( action = action info = 'Checking No conflicts' ).
      try.
          data(lt_conflicting_files) = repository->get_conflicting_files( ).
          if lines( lt_conflicting_files ) > 0 .
            logger->log_error( action = action info = |Conflicts exists for repository| ).
            logger->log_error( action = action info = |No Conflict check failed| ).
            raise exception type cx_cts_abap_vcs_exception
              exporting
                message_variable_1 = zcl_im_gcts_badi=>co_no_conflict_fail.
          endif.
*          "doesn't work properly if registry was used for object relation detection
*          if check_error_in_logs( 'No Conflicts' ) = abap_true.
*            logger->log_error( action = action info = |Errors exists while checking no conflicts| ).
*            raise exception type cx_cts_abap_vcs_exception
*              exporting
*                message_variable_1 = zcl_im_gcts_badi=>co_no_conflict_fail.
*          endif.
        catch cx_cts_abap_vcs_exception.
          logger->log_error( action = action info = |Error while checking for conflicts| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              message_variable_1 = zcl_im_gcts_badi=>co_no_conflict_fail.
      endtry.
    endif.
**************************************
*     Check Changes pulled toggle
**************************************
    if me->changes_pulled_check = abap_true.
      logger->log_info( action = action info = 'Check changes have been pulled' ).
*     If the repository status is not 'CHECKOUT', then check if automatic pull is set, if it is set then the check is a success or else fails.
      if repository->get_config_by_key( key = if_cts_abap_vcs_config_handler=>co_conf_repo_automatic_pull ) = abap_false.
        logger->log_info( action = action info = 'Checking No conflicts' ).
        try.
            technical_status = repository->get_technical_status( ).
            if technical_status-branch_tracking_status-behind_count is not initial and technical_status-branch_tracking_status-behind_count > 0.
              logger->log_error( action = action info = |Behind count for repository : { technical_status-branch_tracking_status-behind_count }| ).
              logger->log_error( action = action info = |Changes have been pulled check failed| ).
              raise exception type cx_cts_abap_vcs_exception
                exporting
                  message_variable_1 = zcl_im_gcts_badi=>co_changes_pulled_fail.
            endif.
          catch cx_cts_abap_vcs_exception.
            logger->log_error( action = action info = |Error while checking for Changes pulled| ).
            raise exception type cx_cts_abap_vcs_exception
              exporting
                message_variable_1 = zcl_im_gcts_badi=>co_changes_pulled_fail.
        endtry.
        if repository->get_status( ) <> if_cts_abap_vcs_repository=>co_repository_status_checkout.
          logger->log_info( action = action info = |Changes have been pulled check failed| ).
          logger->log_info( action = action info = |VCS_AUTOMATIC_PULL value is not set to TRUE| ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              message_variable_1 = zcl_im_gcts_badi=>co_changes_pulled_fail.
        endif.
      endif.
    endif.
**************************************
*     Vsid Fits check
**************************************
    if me->check_vsid = abap_true.
      logger->log_info( action = action info = 'Checking VSID' ).
      if target_system is initial or target_system = ''.
        logger->log_info( action = action info = |Target system for the request is empty| ).
        logger->log_info( action = action info = |Skipping the check for vSID| ).
      else.
        data(create_parameter_object) = cl_cts_tms_tp_parameter_config=>create( ).
        try.
            data(gcts_param) = create_parameter_object->if_cts_tms_tpprofile_parameter~get_parameter_value( sysname = target_system name = if_cts_abap_vcs_system=>co_tms_vcs_system ).
          catch cx_cts_tms_config_exception.
            logger->log_error( action = action info = |Error while retrieving 'NON_ABAP_SYSTEM' TMS parameter. GCTS BAdI cannot be triggered | ).
            raise exception type cx_cts_abap_vcs_exception
              exporting
                message_variable_1 = zcl_im_gcts_badi=>co_non_abap_system_fail.
        endtry.
        if gcts_param = if_cts_abap_vcs_system=>co_tms_vcs_system_value.
          data: vsid_count type i value 0.
          try.
              data(system) = cl_cts_abap_vcs_system_factory=>get_instance( )->get_default_system( ).
              data(repositories) = system->get_repositories_data( refresh = abap_true ).
              loop at repositories into data(ls_repository).
                data(vsid) = ls_repository-vsid.
                if vsid = target_system.
*                  Increase vsid_count by 1. This would be helpful in checking if vsid is unique.
                  vsid_count = vsid_count + 1.
                endif.
              endloop.
              if vsid_count > 1.
*             The vsid is not unique. The badi cannot continue when this is the case for synchronous commits.
                logger->log_error( action = action info = |Non unique Vsid for synchronous commit. Since VSID_CHECK is on, GCTS BAdI cannot be triggered | ).
                raise exception type cx_cts_abap_vcs_exception
                  exporting
                    message_variable_1 = zcl_im_gcts_badi=>co_non_unique_vsid_fail.
              endif.
            catch cx_cts_abap_vcs_exception.
              logger->log_error( action = action info = |Error while retrieving repositories for vsid check toggle. GCTS BAdI cannot be triggered | ).
              raise exception type cx_cts_abap_vcs_exception
                exporting
                  message_variable_1 = zcl_im_gcts_badi=>co_repo_retreival_fail.
          endtry.
        else.
*       The VSID is not of type VCS. THe VSID check would fail in this case.
          logger->log_error( action = action info = |VSID (Target system) is not of type 'VCS' | ).
          raise exception type cx_cts_abap_vcs_exception
            exporting
              message_variable_1 = zcl_im_gcts_badi=>co_vcs_vsid_type_fail.
        endif.
      endif.

    endif.
**************************************
*     Object list evaluation
**************************************
    if me->object_list_eval = abap_true.
****************************************************
*    Evaluate if the objects in the TR are activated
****************************************************
      logger->log_info( action = action info = |Object List evaluation toggle is active| ).
      logger->log_info( action = action info = |Checking if all the objects in TR { tr_number } are activated| ).
      data: ls_es_req      type trwbo_request,
            ls_et_del      type trwbo_t_e070,
            et_messages    type ctsgerrmsgs,
            ev_tr_lock_tst type cts_timestamp,
            ls_e070        type e070,
            lt_log         type standard table of sprot_u.
      ls_es_req-h-trkorr = tr_number.
      call function 'TR_READ_REQUEST'
        exporting
          iv_read_e070       = 'X'
          iv_read_e07t       = 'X'
          iv_read_e070c      = 'X'
          iv_read_e070m      = 'X'
          iv_read_objs_keys  = 'X'
          iv_read_attributes = 'X'
        changing
          cs_request         = ls_es_req
        exceptions
          others             = 1.
      if sy-subrc = 0.
        sort ls_es_req-keys by trkorr pgmid object objname tabkey.
        sort ls_es_req-keys_str by trkorr pgmid object objname tabkey.
        move-corresponding ls_es_req-h to ls_e070.
        call function 'TRINT_CHECK_INACTIVE_OBJECTS'
          exporting
            is_e070 = ls_e070
            it_e071 = ls_es_req-objects
          tables
            et_log  = lt_log.

        read table lt_log transporting no fields
                        with key severity = 'E'.
        if sy-subrc = 0.
*        If the logs contain severity E, then there are inactive objects in the transport request.
          logger->log_error( action = action info = |Inactive objects are there in the transport request | ).
          loop at lt_log into data(ls_log).
            logger->log_error( action = action info = |Severity : { ls_log-severity }| ).
            logger->log_error( action = action info = |Inactive Object details : { ls_log-var1 }, { ls_log-var2 } | ).
          endloop.
          raise exception type cx_cts_abap_vcs_exception
            exporting
              message_variable_1 = zcl_im_gcts_badi=>co_inactive_object_fail.
        else.
          logger->log_info( action = action info = |The Transport request contains active objects, Object list evaluation passed| ).
        endif.
      else.
        logger->log_error( action = action info = |Error while trying to retrieve request for object activation check| ).
        raise exception type cx_cts_abap_vcs_exception
          exporting
            message_variable_1 = zcl_im_gcts_badi=>co_inactive_object_fail.
      endif.
      logger->log_info( action = action info = |All objects are activated.| ).
***************************************************
*    Check if the objects are there in multiple TRs
***************************************************
      logger->log_info( action = action info = |Check if the objects are in multiple transport requests| ).
      data: lv_trkorr  type strkorr.
      data: lt_tlock        type standard table of tlock,
            ls_tadir_object type tadir.
      if ls_es_req-objects is not initial.
        loop at ls_es_req-objects into data(ls_req_objects).
          if ls_req_objects-pgmid = 'LIMU'.
*          If the object is a LIMU then check if the parent object lock check
            call function 'TR_CHECK_TYPE'
              exporting
                wi_e071  = ls_req_objects
              importing
                we_tadir = ls_tadir_object
              exceptions
                others   = 1.

            if sy-subrc = 0.
              ls_req_objects-pgmid = ls_tadir_object-pgmid.
              ls_req_objects-object = ls_tadir_object-object.
              ls_req_objects-obj_name = ls_tadir_object-obj_name.
              clear ls_tadir_object.
            endif.
          endif.
          call function 'TR_CHECK_OBJECT_LOCK'
            exporting
              wi_pgmid             = ls_req_objects-pgmid
              wi_object            = ls_req_objects-object
              wi_objname           = ls_req_objects-obj_name
            tables
              wt_tlock             = lt_tlock
            exceptions
              empty_key            = 1
              no_systemname        = 2
              no_systemtype        = 3
              unallowed_lock_order = 4
              others               = 5.
          clear lv_trkorr.
          if sy-subrc = 0 and not lt_tlock is initial.
            loop at lt_tlock into data(ls_tlock).
              select single * from e070 into @data(ls_e070_table) where trkorr = @ls_tlock-trkorr.
              if sy-subrc = 0.
                if ls_e070_table-strkorr is not initial and lv_trkorr is initial.
                  lv_trkorr = ls_e070_table-strkorr.
                elseif ls_e070_table-strkorr is not initial and lv_trkorr is not initial.
                  if lv_trkorr <> ls_e070_table-strkorr.
*                 The object exists in two different transport requests. Hence the release is not possible.
                    logger->log_error( action = action info = |Object: { ls_req_objects-object } { ls_req_objects-obj_name } is part of { lv_trkorr } and { ls_e070_table-strkorr }| ).
                    raise exception type cx_cts_abap_vcs_exception
                      exporting
                        message_variable_1 = zcl_im_gcts_badi=>co_multiple_tr_fail.
                  endif.
                endif.
              endif.
            endloop.
          endif.
        endloop.
      else.
        logger->log_info( action = action info = |Could not retrieve any objects in the Transport request| ).
      endif.
    endif.
    is_eval_success = abap_true.
    logger->log_end( action = action info = 'End of Action' ).
  endmethod.

  method evaluation_toggle_information.
    data: ls_toggle type ty_toggle_info.
    ls_toggle-toggle_name = 'rep_status'.
    if info cs 'Repository_status'.
      ls_toggle-toggle_value = abap_true.
    else.
      ls_toggle-toggle_value = abap_false.
    endif.
    append ls_toggle to toggles.
    clear ls_toggle.
  endmethod.

  method check_error_in_logs.
    data(action) = co_repository_eval_errors.
    logger->log_info( action = action info = |Checking errors in logs for operation : { operation_constant } | ).
    data(lt_log) = cl_cts_abap_vcs_logger=>if_cts_abap_vcs_logger~get_log( abap_true ).
    if lines( lt_log ) > 0.

*         Checking the logs prefereably from the java client if there are any errors
*         The logs needs to be analysed since that could also provide information if there are any errors
*         The logs cannot be flushed because then badi logs are also lost along with the flush.
      logger->log_error( action = action info = |Error while retrieving technical status from the git client| ).
      loop at lt_log into data(ls_log).
        logger->log_error( action = action info = |Code: { ls_log-code } Message: { ls_log-message }| ).
      endloop.
      errors_exists = abap_true.
      return.
    endif.
  endmethod.

endclass.

class ltcl_gcts_branch_handling definition.
  public section.
*   Constant for logging the registry functions class.
    constants co_branch_handling type string value 'GIT_FORWARD_BADI_BRANCH_HANDLING'.
*   Constants for the branch dynamic calculation
    constants: co_branch_release   type string value '{release}',
               co_branch_attribute type string value '{attribute}',
               co_sap_basis        type string value 'SAP_BASIS',
               co_git_branch       type string value 'GIT_BRANCH',
               co_parameter        type string value '{parameter}',
               co_cvers            type string value 'CVERS',
               co_component        type string value 'COMPONENT'.
*   Constants for logging. Each constant is for a certain action/method
    constants: co_retreive_branch_info    type string value 'RETRIEVE_BRANCH_INFO',
               co_handle_branch_operation type string value 'HANDLE_BRANCH_OPERATION'.
    data:      logger  type ref to if_cts_abap_vcs_logger.
    types: begin of branch_info_parameter,
             table  type string,
             column type string,
             key    type string,
           end of branch_info_parameter.
    types: begin of branch_info_syntax,
             syntax    type string,
             parameter type branch_info_parameter,
           end of branch_info_syntax.
    methods constructor
      importing create_branch_toggle type abap_boolean
                branch_handling      type abap_boolean
                branch_handling_info type string.

    methods retreive_branch_info
      importing transport_number type trkorr
      returning value(branch)    type scts_abap_vcs_branch_name.

    methods handle_branch_operation
      importing repository_id        type scts_abap_vcs_repository_id
                repository           type ref to if_cts_abap_vcs_repository
                new_branch           type scts_abap_vcs_branch_name
      returning value(is_successful) type abap_boolean.

  private section.
    data: create_branch_toggle type abap_boolean,
          branch_handling      type abap_boolean,
          branch_handling_info type string.
endclass.

class ltcl_gcts_branch_handling implementation.
  method constructor.
    logger ?= new cl_cts_abap_vcs_logger( section = co_branch_handling ).
    me->create_branch_toggle = create_branch_toggle.
    me->branch_handling = branch_handling.
    me->branch_handling_info = branch_handling_info.
  endmethod.

  method handle_branch_operation.
    data(action) = co_handle_branch_operation.
    logger->log_start( action = action ).
    data(current_branch) = repository->get_branch(  ).
    logger->log_info( action = action info = |Current branch is : { current_branch } | ).
    if current_branch <> new_branch.
      try.
          data(repository_branches) = repository->get_branches(  ).
        catch cx_cts_abap_vcs_exception.
          logger->log_error( action = action info = |Error while retrieving all branches of repository { repository_id } | ).
          is_successful = abap_false.
          return.
      endtry.
      if line_exists( repository_branches[ name = new_branch ] ) and me->branch_handling = abap_true.
        data: switch_branch_request  type cl_cts_abap_vcs_branch_facade=>ty_switch_branch_request .
        logger->log_info( action = action info = |Branch { new_branch } is part of repository, Switching branch| ).
        switch_branch_request-repository = repository_id.
        switch_branch_request-branch = new_branch.
        logger->log_info( action = action info = |Switching branch of repository { repository_id } from { current_branch } to { new_branch } | ).
        data(ls_switch_response) = cl_cts_abap_vcs_branch_facade=>switch_branch( request = switch_branch_request ).
        if ls_switch_response-result is initial or ls_switch_response-error_log is not initial or ls_switch_response-exception is not initial.
          logger->log_error( action = action info = |Switch of branch failed| ).
          logger->log_error( action = action info = |Exception : { ls_switch_response-exception } | ).
          is_successful = abap_false.
          return.
        endif.
      elseif not line_exists( repository_branches[ name = new_branch ] ) and me->branch_handling = abap_true
                 and me->create_branch_toggle = abap_true.
        logger->log_info( action = action info = |Branch { new_branch } is not part of repository, Creating branch { new_branch } globally| ).
        data: create_branch_request type cl_cts_abap_vcs_branch_facade=>ty_create_branch_request .
        create_branch_request-branch = new_branch.
        create_branch_request-repository = repository_id.
        create_branch_request-type = 'global'.
        data(ls_create_response) = cl_cts_abap_vcs_branch_facade=>create_branch( request = create_branch_request ).
        if ls_create_response-branch is initial or ls_create_response-error_log is not initial or ls_create_response-exception is not initial.
          logger->log_error( action = action info = |Creation of branch failed| ).
          logger->log_error( action = action info = |Exception : { ls_create_response-exception } | ).
          is_successful = abap_false.
          return.
        endif.
      else.
        logger->log_info( action = action info = |Switch branch toggle off, switch to branch { new_branch } from { current_branch } ignored | ).
*       The switch did not happen, should the branch be retained and push continued ?
        is_successful = abap_true.
        return.
      endif.
      is_successful = abap_true.
    endif.
  endmethod.

  method retreive_branch_info.
    data(action) = co_retreive_branch_info.
    logger->log_start( action = action ).
    logger->log_info( action = action info = |Branch handling toggle : { branch_handling } | ).
    logger->log_info( action = action info = |Branch handling info : { branch_handling_info } | ).
    data: ls_branch_info_syntax type branch_info_syntax.
    if branch_handling = abap_true and branch_handling_info is not initial.
      data(dynamic_branch_value) = match( val = branch_handling_info pcre = `\{.+\}` ).
      if dynamic_branch_value is initial or dynamic_branch_value = ''.
*         Static branch name
        branch = branch_handling_info.
      elseif dynamic_branch_value = co_branch_release.
        select single * from cvers into @data(ls_cvers) where component = @co_sap_basis.
        if sy-subrc = 0.
          branch = replace( val = branch_handling_info sub = co_branch_release with = ls_cvers-release ).
        endif.
      elseif dynamic_branch_value = co_branch_attribute.
        select single * from e070 into @data(ls_e070) where trkorr = @transport_number.
        if sy-subrc = 0 and ls_e070-strkorr is not initial.
          logger->log_info( action = action info = |Parent TR for retrieving GIT_BRANCH: { ls_e070-strkorr } | ).
          select single * from e070a into @data(ls_e070a) where trkorr = @ls_e070-strkorr and attribute = @co_git_branch.
          if sy-subrc = 0.
            branch = replace( val = branch_handling_info sub = co_branch_attribute with = ls_e070a-reference ).
          endif.
        endif.
      else.
        /ui2/cl_json=>deserialize(
         exporting
           json = branch_handling_info
           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         changing
           data = ls_branch_info_syntax
       ).
        if ls_branch_info_syntax-syntax is not initial or ls_branch_info_syntax-parameter is not initial.
          if ls_branch_info_syntax-parameter-table = co_cvers and ls_branch_info_syntax-parameter-column = co_component.
            select single * from cvers into @ls_cvers where component = @ls_branch_info_syntax-parameter-key.
            if sy-subrc = 0.
              branch = replace( val = ls_branch_info_syntax-syntax sub = co_parameter with = ls_cvers-release ).
            endif.
          endif.
        endif.
      endif.
    endif.
    logger->log_info( action = action info = |Branch calculated from info : { branch } | ).
    logger->log_end( action = action ).
  endmethod.

endclass.

class ltcl_gcts_feature_toggle definition.
  public section.
*   Constant for logging the feature toggle class.
    constants co_feature_toggle type string value 'GIT_FORWARD_BADI_FEATURE_TOGGLES'.
    types: begin of ty_feature_toggles,
             toggle          type string,
             value           type abap_boolean,
             additional_info type string,
             level           type string,
           end of ty_feature_toggles.
    types: begin of ty_object_resolvement_info,
             strategy type string,
             level    type string,
           end of ty_object_resolvement_info.
    data:      logger  type ref to if_cts_abap_vcs_logger.
    types tt_feature_toggles type standard table of ty_feature_toggles with default key.
    types: begin of ty_functionalities,
             general_functions     type ref to ltcl_gcts_general_functions,
             repository_evaluation type ref to ltcl_gcts_repository_eval,
             badi_trigger          type ref to ltcl_gcts_badi_trigger,
             branch_handling       type ref to ltcl_gcts_branch_handling,
           end of ty_functionalities.
    methods get_feature_toggles
      returning value(rt_feature_toggles) type tt_feature_toggles.
    methods set_badi_toggles
      returning value(rs_functionalities) type ty_functionalities.
    methods constructor.
    methods get_badi_trigger_toggles
      returning value(badi_toggle_structure) type ty_feature_toggles.
    methods get_branch_handling_toggles
      returning value(rs_branch_handling_toggle) type ty_feature_toggles.
    methods get_relation_ident_inf_toggles
      returning value(rs_relation_ident_info_toggle) type ty_feature_toggles.
  protected section.
  private section.
    data: badi_trigger_feature_toggle type ty_feature_toggles,
          branch_handling_toggle      type ty_feature_toggles,
          relation_ident_info_toggle  type ty_feature_toggles.

endclass.

class ltcl_gcts_feature_toggle implementation.

  method get_feature_toggles.
    field-symbols: <lt_toggle_table>   type standard table,
                   <ls_toggle_table>   type any,
                   <toggle>            type any,
                   <value>             type any,
                   <additional_info>   type any,
                   <temp_column_value> type any.
    data: o_table_ref type ref to data,
          ls_toggle   type ty_feature_toggles.
    if zcl_im_gcts_badi=>co_toggle_mview is not initial.
      create data o_table_ref type table of (zcl_im_gcts_badi=>co_toggle_mview).
      assign zcl_im_gcts_badi=>p_toggle_column to <toggle>.
      assign zcl_im_gcts_badi=>p_value_column to <value>.
      assign zcl_im_gcts_badi=>p_value_additional_info to <additional_info>.
      assign o_table_ref->* to <lt_toggle_table>.
      select * into table <lt_toggle_table> from (zcl_im_gcts_badi=>co_toggle_mview).
      loop at <lt_toggle_table> assigning <ls_toggle_table>.
        clear ls_toggle.
        assign component <toggle> of structure <ls_toggle_table> to <temp_column_value>.
        ls_toggle-toggle = <temp_column_value>.
        clear <temp_column_value>.
        assign component <value> of structure <ls_toggle_table> to <temp_column_value>.
        ls_toggle-value = <temp_column_value>.
        clear <temp_column_value>.
        assign component <additional_info> of structure <ls_toggle_table> to <temp_column_value>.
        ls_toggle-additional_info = <temp_column_value>.
        clear <temp_column_value>.
        append ls_toggle to rt_feature_toggles.
      endloop.
    endif.
  endmethod.

  method set_badi_toggles.
    data(toggles) = me->get_feature_toggles( ).
*   For logging the toggles set in the maintenance view. This can be set in SM30 with view zgcts_badi_toggl.
    loop at toggles into data(ls_toggles).
      logger->log_info( action = 'SET_BADI_TOGGLES' info = |toggle: { ls_toggles-toggle } value: { ls_toggles-value }, additional info : { ls_toggles-additional_info }| ).
    endloop.

    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_async_commit ] ).
      data(async_commit) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_async_commit ]-value.
    else.
      async_commit = zcl_im_gcts_badi=>co_async_commit.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_authentication_check ] ).
      data(auth_check) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_authentication_check ]-value.
    else.
      auth_check = zcl_im_gcts_badi=>co_authentication_check.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_target_system_check ] ).
      data(target_system_check) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_target_system_check ]-value.
    else.
      target_system_check = zcl_im_gcts_badi=>co_target_system_check.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_user_permission ] ).
      data(user_permission) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_user_permission ]-value.
    else.
      user_permission = zcl_im_gcts_badi=>co_user_permission.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_relation_identifier ] ).
      data(registry_toggle) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_relation_identifier ]-value.
      data(registry_toggle_info) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_relation_identifier ]-additional_info.
    else.
*    If toggle is not set for registry, then the default behaviour is for matching with the objects registered using registry
      registry_toggle = zcl_im_gcts_badi=>co_registry_toggle.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_warning_level_toggle ] ).
      data(warning_level_toggle) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_warning_level_toggle ]-value.
      data(warning_level_info) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_warning_level_toggle ]-additional_info.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_unique_repository ] ).
      data(unique_repository) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_unique_repository ]-value.
      data(unique_repository_info) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_unique_repository ]-additional_info.
    else.
      unique_repository = zcl_im_gcts_badi=>co_unique_repository.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_badi_trigger ] ).
      data(badi_trigger) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_badi_trigger ]-value.
      data(badi_trigger_info) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_badi_trigger ]-additional_info.
    else.
      badi_trigger = zcl_im_gcts_badi=>co_badi_trigger.
    endif.
*    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_repo_status_check ] ).
*      data(repo_status_check) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_repo_status_check ]-value.
*      data(repo_status_check_info) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_repo_status_check ]-additional_info.
*    else.
*      repo_status_check = zcl_im_gcts_badi=>co_repo_status_check.
*    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_tech_status_check ] ).
      data(tech_status_check) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_tech_status_check ]-value.
      data(tech_status_check_info) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_tech_status_check ]-additional_info.
    else.
      tech_status_check = zcl_im_gcts_badi=>co_tech_status_check.
    endif.
*    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_no_conflicts_check ] ).
*      data(no_conflicts_check) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_no_conflicts_check ]-value.
*    else.
*      tech_status_check = zcl_im_gcts_badi=>co_no_conflicts_check.
*    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_changes_pulled_ch ] ).
      data(changes_pulled_check) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_changes_pulled_ch ]-value.
    else.
      changes_pulled_check = zcl_im_gcts_badi=>co_changes_pulled_check.
    endif.
*    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_user_auth_check ] ).
*      data(user_auth_check) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_user_auth_check ]-value.
*    else.
*      user_auth_check = zcl_im_gcts_badi=>co_user_authentication_check.
*    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_check_vsid ] ).
      data(check_vsid) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_check_vsid ]-value.
    else.
      check_vsid = zcl_im_gcts_badi=>co_check_vsid.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_object_list_eval ] ).
      data(object_list_eval) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_object_list_eval ]-value.
    else.
      object_list_eval = zcl_im_gcts_badi=>co_object_list_eval.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_branch_handling ] ).
      data(branch_handling) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_branch_handling ]-value.
      data(branch_handling_info) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_branch_handling ]-additional_info.
    else.
      branch_handling = zcl_im_gcts_badi=>co_branch_handling.
    endif.
    if line_exists( toggles[ toggle = zcl_im_gcts_badi=>co_nm_create_branch_toggle ] ).
      data(create_branch_toggle) = toggles[ toggle = zcl_im_gcts_badi=>co_nm_create_branch_toggle ]-value.
    else.
      create_branch_toggle = zcl_im_gcts_badi=>co_create_branch_toggle.
    endif.

    data: ls_object_resolvement_info type ty_object_resolvement_info.

    /ui2/cl_json=>deserialize(
           exporting
             json = registry_toggle_info
             pretty_name = /ui2/cl_json=>pretty_mode-camel_case
           changing
             data = ls_object_resolvement_info
         ).
*   Set Object resolvement toggle. This is required to later use it for resolving the object to a repository.
*   Based on the level a warning or info is thrown when there no matches found for any of the objects in the TR.
    me->relation_ident_info_toggle-toggle = zcl_im_gcts_badi=>co_nm_relation_identifier.
    me->relation_ident_info_toggle-value = registry_toggle.
    me->relation_ident_info_toggle-additional_info = ls_object_resolvement_info-strategy.
    me->relation_ident_info_toggle-level = ls_object_resolvement_info-level.
*   Change the superpackage behaviour, based on the registry task
    rs_functionalities-general_functions = new ltcl_gcts_general_functions( package = zcl_im_gcts_badi=>co_superpackage
                                                                         commit_async_toggle = async_commit
                                                                         user_permission_toggle = user_permission
                                                                         authentication_toggle = auth_check
                                                                         target_system_check_toggle = target_system_check
                                                                         object_resolvement_toggle = registry_toggle
                                                                         object_resolvement_info = ls_object_resolvement_info-strategy
                                                                         warning_level_toggle = warning_level_toggle
                                                                         warning_level_info = warning_level_info
                                                                         unique_repository = unique_repository
                                                                         unique_repository_info = unique_repository_info
                                                                         badi_trigger = badi_trigger
                                                                         badi_trigger_info = badi_trigger_info
                                                                         object_resolvement_level = ls_object_resolvement_info-level ).

*   Set the repository evaluation object
    rs_functionalities-repository_evaluation = new ltcl_gcts_repository_eval( async_commit = async_commit
                                                                              changes_pulled_check = changes_pulled_check
                                                                              check_vsid = check_vsid
                                                                              object_list_eval = object_list_eval
                                                                              tech_status_check = tech_status_check ).

* Set the branch handling object
    rs_functionalities-branch_handling = new ltcl_gcts_branch_handling( create_branch_toggle = create_branch_toggle
                                                                        branch_handling = branch_handling
                                                                        branch_handling_info = branch_handling_info ).

*   Set badi trigger toggle
    me->badi_trigger_feature_toggle-toggle = zcl_im_gcts_badi=>co_nm_badi_trigger.
    me->badi_trigger_feature_toggle-value = badi_trigger.
    me->badi_trigger_feature_toggle-additional_info = badi_trigger_info.

*   Set Branch handling toggle
    me->branch_handling_toggle-toggle = zcl_im_gcts_badi=>co_nm_branch_handling.
    me->branch_handling_toggle-value = branch_handling.
    me->branch_handling_toggle-additional_info = branch_handling_info.

  endmethod.

  method constructor.
    logger ?= new cl_cts_abap_vcs_logger( section = co_feature_toggle ).
  endmethod.

  method get_badi_trigger_toggles.
    badi_toggle_structure = me->badi_trigger_feature_toggle.
  endmethod.

  method get_branch_handling_toggles.
    rs_branch_handling_toggle = me->branch_handling_toggle.
  endmethod.

  method get_relation_ident_inf_toggles.
    rs_relation_ident_info_toggle = me->relation_ident_info_toggle.
  endmethod.

endclass.