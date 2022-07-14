class zcl_im_gcts_badi definition
  public
  final
  create public .

  public section.
    types ty_tadir type standard table of tadir with default key.
    types ty_objects_to_push type table of if_cts_abap_vcs_transport_req=>ty_objects with default key.
    interfaces if_ex_cts_request_check .
    constants co_superpackage type string value '' ##NO_TEXT.
*  toggle constants. Should only be set when the necessary toggle are required
    constants: co_async_commit              type boolean value abap_false,
               co_user_permission           type boolean value abap_false,
               co_target_system_check       type boolean value abap_false,
               co_authentication_check      type boolean value abap_false,
               co_registry_toggle           type boolean value abap_true,
               co_unique_repository         type boolean value abap_false,
               co_badi_trigger              type boolean value abap_false,
               co_repo_status_check         type boolean value abap_true,
               co_tech_status_check         type boolean value abap_true,
               co_warning_level_toggle      type boolean value abap_false,
               co_no_conflicts_check        type boolean value abap_true,
               co_changes_pulled_check      type boolean value abap_true,
               co_user_authentication_check type boolean value abap_true,
               co_check_vsid                type boolean value abap_true,
               co_object_list_eval          type boolean value abap_true,
               co_branch_handling           type boolean value abap_false,
               co_create_branch_toggle      type boolean value abap_false.
*  Constant for the toggle names
    constants: co_nm_async_commit         type string value 'REPOSITORY_ASYNC_COMMIT',
               co_nm_authentication_check type string value 'CHECK_PERMISSION',
               co_nm_target_system_check  type string value 'CHECK_REPOSITORY_TARGET_SYSTEM',
               co_nm_user_permission      type string value 'CHECK_REPOSITORY_USER_PERMISSION',
               co_nm_relation_identifier  type string value 'REPOSITORY_RELATION_IDENTIFIER',
               co_nm_warning_level_toggle type string value 'WARNING_LEVEL_TOGGLE',
               co_nm_unique_repository    type string value 'CHECK_UNIQUE_REPOSITORY',
               co_nm_badi_trigger         type string value 'BADI_GCTS_ENABLE_TRIGGER',
               co_nm_tech_status_check    type string value 'CHECK_REPOSITORY_STATUS',
               co_nm_changes_pulled_ch    type string value 'CHECK_REPOSITORY_CHANGES_PULLED',
               co_nm_check_vsid           type string value 'CHECK_REPOSITORY_VSID',
               co_nm_object_list_eval     type string value 'CHECK_REPOSITORY_OBJECT_LIST',
               co_nm_branch_handling      type string value 'BRANCH_HANDLING_ENABLE',
               co_nm_create_branch_toggle type string value 'BRANCH_CREATE_TOGGLE'.
*  info for the different toggles. The table is checked if a value exists. If it does not retrieve from the constant
    constants: co_registry_info          type string value 'STATIC',
               co_warning_level_info     type string value 'DEFAULT',
               co_unique_repository_info type string value '',
               co_badi_trigger_info      type string value 'VSID_TR'.
*  Constants defining the maintenance table which store the toggle values
    constants: co_toggle_mview             type tabname value 'zgcts_badi_toggl',
               p_toggle_column(20)         type c value 'feature',
               p_value_column(20)          type c value 'flag',
               p_value_additional_info(20) type c value 'additional_info'.
*   Constants for the different levels of handling during the object resolvement using different strategies
    constants: co_level_warning type string value 'warning',
               co_level_info    type string value 'info',
               co_level_error   type string value 'error'.
*   Exception constants
    constants: co_repo_status_fail     type symsgv  value 'Repository Status check failed',
               co_status_fail          type symsgv  value 'Status check failed',
               co_no_conflict_fail     type symsgv  value 'Check for non existence of Conflicts failed',
               co_changes_pulled_fail  type symsgv  value 'Changes have been pulled check failed',
               co_non_abap_system_fail type symsgv  value 'Error when getting NON_ABAP_SYSTEM TMS parameter',
               co_non_unique_vsid_fail type symsgv  value 'Non unique Vsid for synchronous commit',
               co_repo_retreival_fail  type symsgv  value 'Error while getting repos for vsid check toggle',
               co_vcs_vsid_type_fail   type symsgv  value 'VSID (Target system) is not of type VCS',
               co_inactive_object_fail type symsgv  value 'Inactive objects in the transport request',
               co_multiple_tr_fail     type symsgv  value 'Object are locked in multiple transport requests'.



