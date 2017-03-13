pragma Ada_95;
pragma Warnings (Off);
with System;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 6.2.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_caesar" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#d088ba27#;
   pragma Export (C, u00001, "caesarB");
   u00002 : constant Version_32 := 16#b6df930e#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#36a12203#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#40c46368#;
   pragma Export (C, u00005, "ada__command_lineB");
   u00006 : constant Version_32 := 16#d59e21a4#;
   pragma Export (C, u00006, "ada__command_lineS");
   u00007 : constant Version_32 := 16#c6f79445#;
   pragma Export (C, u00007, "systemS");
   u00008 : constant Version_32 := 16#0f0cb66d#;
   pragma Export (C, u00008, "system__secondary_stackB");
   u00009 : constant Version_32 := 16#6d965b2c#;
   pragma Export (C, u00009, "system__secondary_stackS");
   u00010 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00010, "system__parametersB");
   u00011 : constant Version_32 := 16#b8dd993a#;
   pragma Export (C, u00011, "system__parametersS");
   u00012 : constant Version_32 := 16#465d427a#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#58734c10#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#42f0179b#;
   pragma Export (C, u00014, "ada__exceptionsB");
   u00015 : constant Version_32 := 16#a7decac7#;
   pragma Export (C, u00015, "ada__exceptionsS");
   u00016 : constant Version_32 := 16#e947e6a9#;
   pragma Export (C, u00016, "ada__exceptions__last_chance_handlerB");
   u00017 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00017, "ada__exceptions__last_chance_handlerS");
   u00018 : constant Version_32 := 16#87a448ff#;
   pragma Export (C, u00018, "system__exception_tableB");
   u00019 : constant Version_32 := 16#9b59fd07#;
   pragma Export (C, u00019, "system__exception_tableS");
   u00020 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00020, "system__exceptionsB");
   u00021 : constant Version_32 := 16#ae94f9b3#;
   pragma Export (C, u00021, "system__exceptionsS");
   u00022 : constant Version_32 := 16#4c9e814d#;
   pragma Export (C, u00022, "system__exceptions__machineS");
   u00023 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00023, "system__exceptions_debugB");
   u00024 : constant Version_32 := 16#b87d6d81#;
   pragma Export (C, u00024, "system__exceptions_debugS");
   u00025 : constant Version_32 := 16#570325c8#;
   pragma Export (C, u00025, "system__img_intB");
   u00026 : constant Version_32 := 16#c42c7487#;
   pragma Export (C, u00026, "system__img_intS");
   u00027 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00027, "system__storage_elementsB");
   u00028 : constant Version_32 := 16#eb34de41#;
   pragma Export (C, u00028, "system__storage_elementsS");
   u00029 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00029, "system__tracebackB");
   u00030 : constant Version_32 := 16#98d54a81#;
   pragma Export (C, u00030, "system__tracebackS");
   u00031 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00031, "system__traceback_entriesB");
   u00032 : constant Version_32 := 16#c6ac6235#;
   pragma Export (C, u00032, "system__traceback_entriesS");
   u00033 : constant Version_32 := 16#6fd210f2#;
   pragma Export (C, u00033, "system__traceback__symbolicB");
   u00034 : constant Version_32 := 16#dd19f67a#;
   pragma Export (C, u00034, "system__traceback__symbolicS");
   u00035 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00035, "ada__exceptions__tracebackB");
   u00036 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00036, "ada__exceptions__tracebackS");
   u00037 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00037, "system__address_imageB");
   u00038 : constant Version_32 := 16#671b097f#;
   pragma Export (C, u00038, "system__address_imageS");
   u00039 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00039, "system__wch_conB");
   u00040 : constant Version_32 := 16#dd8ab697#;
   pragma Export (C, u00040, "system__wch_conS");
   u00041 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00041, "system__wch_stwB");
   u00042 : constant Version_32 := 16#f09b9a96#;
   pragma Export (C, u00042, "system__wch_stwS");
   u00043 : constant Version_32 := 16#b96cfbef#;
   pragma Export (C, u00043, "system__wch_cnvB");
   u00044 : constant Version_32 := 16#d23d0c64#;
   pragma Export (C, u00044, "system__wch_cnvS");
   u00045 : constant Version_32 := 16#4be8ce1b#;
   pragma Export (C, u00045, "interfacesS");
   u00046 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00046, "system__wch_jisB");
   u00047 : constant Version_32 := 16#524d1545#;
   pragma Export (C, u00047, "system__wch_jisS");
   u00048 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00048, "system__stack_checkingB");
   u00049 : constant Version_32 := 16#4848ffad#;
   pragma Export (C, u00049, "system__stack_checkingS");
   u00050 : constant Version_32 := 16#c1a106e2#;
   pragma Export (C, u00050, "ada__text_ioB");
   u00051 : constant Version_32 := 16#28a21868#;
   pragma Export (C, u00051, "ada__text_ioS");
   u00052 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00052, "ada__streamsB");
   u00053 : constant Version_32 := 16#2e6701ab#;
   pragma Export (C, u00053, "ada__streamsS");
   u00054 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00054, "ada__io_exceptionsS");
   u00055 : constant Version_32 := 16#920eada5#;
   pragma Export (C, u00055, "ada__tagsB");
   u00056 : constant Version_32 := 16#13ca27f3#;
   pragma Export (C, u00056, "ada__tagsS");
   u00057 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00057, "system__htableB");
   u00058 : constant Version_32 := 16#423527af#;
   pragma Export (C, u00058, "system__htableS");
   u00059 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00059, "system__string_hashB");
   u00060 : constant Version_32 := 16#e06b4cd1#;
   pragma Export (C, u00060, "system__string_hashS");
   u00061 : constant Version_32 := 16#5baf3085#;
   pragma Export (C, u00061, "system__unsigned_typesS");
   u00062 : constant Version_32 := 16#b44f9ae7#;
   pragma Export (C, u00062, "system__val_unsB");
   u00063 : constant Version_32 := 16#4b07ddbe#;
   pragma Export (C, u00063, "system__val_unsS");
   u00064 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00064, "system__val_utilB");
   u00065 : constant Version_32 := 16#6a5722bb#;
   pragma Export (C, u00065, "system__val_utilS");
   u00066 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00066, "system__case_utilB");
   u00067 : constant Version_32 := 16#e2fefd92#;
   pragma Export (C, u00067, "system__case_utilS");
   u00068 : constant Version_32 := 16#84a27f0d#;
   pragma Export (C, u00068, "interfaces__c_streamsB");
   u00069 : constant Version_32 := 16#a06e9ee4#;
   pragma Export (C, u00069, "interfaces__c_streamsS");
   u00070 : constant Version_32 := 16#b666424b#;
   pragma Export (C, u00070, "system__crtlS");
   u00071 : constant Version_32 := 16#f1dc49a7#;
   pragma Export (C, u00071, "system__file_ioB");
   u00072 : constant Version_32 := 16#61867520#;
   pragma Export (C, u00072, "system__file_ioS");
   u00073 : constant Version_32 := 16#cf417de3#;
   pragma Export (C, u00073, "ada__finalizationS");
   u00074 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00074, "system__finalization_rootB");
   u00075 : constant Version_32 := 16#8905e7d5#;
   pragma Export (C, u00075, "system__finalization_rootS");
   u00076 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00076, "interfaces__cB");
   u00077 : constant Version_32 := 16#61e3d2ff#;
   pragma Export (C, u00077, "interfaces__cS");
   u00078 : constant Version_32 := 16#1ca2762f#;
   pragma Export (C, u00078, "system__os_libB");
   u00079 : constant Version_32 := 16#dc0cac3f#;
   pragma Export (C, u00079, "system__os_libS");
   u00080 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00080, "system__stringsB");
   u00081 : constant Version_32 := 16#b8488523#;
   pragma Export (C, u00081, "system__stringsS");
   u00082 : constant Version_32 := 16#3b680eed#;
   pragma Export (C, u00082, "system__file_control_blockS");
   u00083 : constant Version_32 := 16#7ebd8839#;
   pragma Export (C, u00083, "system__val_intB");
   u00084 : constant Version_32 := 16#8e52be7a#;
   pragma Export (C, u00084, "system__val_intS");
   u00085 : constant Version_32 := 16#58e7cff7#;
   pragma Export (C, u00085, "system__memoryB");
   u00086 : constant Version_32 := 16#9f8af271#;
   pragma Export (C, u00086, "system__memoryS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.command_line%s
   --  interfaces%s
   --  system%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.htable%s
   --  system.img_int%s
   --  system.img_int%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.val_int%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_int%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  system.address_image%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.secondary_stack%s
   --  system.file_io%b
   --  interfaces.c%b
   --  ada.tags%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  ada.command_line%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  ada.exceptions.traceback%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  caesar%b
   --  END ELABORATION ORDER


end ada_main;
