*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04.05.2022 at 10:56:02
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZGCTS_BADI_TOGGL................................*
DATA:  BEGIN OF STATUS_ZGCTS_BADI_TOGGL              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGCTS_BADI_TOGGL              .
CONTROLS: TCTRL_ZGCTS_BADI_TOGGL
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGCTS_BADI_TOGGL              .
TABLES: ZGCTS_BADI_TOGGL               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .