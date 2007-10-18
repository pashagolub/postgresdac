unit PSQLConsts;

interface

const

  sqlNoSysObjects  = ' AND n.nspname NOT IN (''pg_catalog'', ''pg_toast'','+
                    '''pg_sysviews'', ''information_schema'')';

  sqlShowTablesPrivelegesWithGrantOptsEx  = 'SELECT c.relname, n.nspname,                           '+
                           ' CASE                                       '+
                           '      WHEN c.relkind=''r'' THEN ''<Table>'' '+
                           '      ELSE ''<View>''                        '+
                           ' END::varchar(7) AS relkind,                            '+
                           ' has_table_privilege(''%s'',c.oid,''SELECT'') AS SEL,                  '+
                           ' has_table_privilege(''%s'',c.oid,''INSERT'') AS INS,                  '+
                           ' has_table_privilege(''%s'',c.oid,''UPDATE'') AS UPD,                  '+
                           ' has_table_privilege(''%s'',c.oid,''DELETE'') AS DEL,                  '+
                           ' has_table_privilege(''%s'',c.oid,''RULE'') AS RUL,                    '+
                           ' has_table_privilege(''%s'',c.oid,''REFERENCES'') AS REF,              '+
                           ' has_table_privilege(''%s'',c.oid,''TRIGGER'') AS TRIG, '+
                           ' has_table_privilege(''%s'',c.oid,''SELECT WITH GRANT OPTION'') AS SEL_GRANT,'+
                           ' has_table_privilege(''%s'',c.oid,''INSERT WITH GRANT OPTION'') AS INS_GRANT,'+
                           ' has_table_privilege(''%s'',c.oid,''UPDATE WITH GRANT OPTION'') AS UPD_GRANT,'+
                           ' has_table_privilege(''%s'',c.oid,''DELETE WITH GRANT OPTION'') AS DEL_GRANT,'+
                           ' has_table_privilege(''%s'',c.oid,''RULE WITH GRANT OPTION'') AS RUL_GRANT ,'+
                           ' has_table_privilege(''%s'',c.oid,''REFERENCES WITH GRANT OPTION'') AS REF_GRANT,'+
                           ' has_table_privilege(''%s'',c.oid,''TRIGGER WITH GRANT OPTION'') AS TRIG_GRANT'+
                           ' FROM pg_class AS c, pg_namespace AS n                          '+
                           ' WHERE c.relnamespace = n.oid                                   '+
                           ' AND c.relkind IN (''r'',''v'') %NoSys                          ';

  sqlShowDatabasePrivelegesWithGrantOptsEx  = 'SELECT datname,                               '+
                           ' has_database_privilege(''%s'',datname,''CREATE'') as CREATE,'+
                           ' has_database_privilege(''%s'',datname,''TEMP'') as TEMP,  '+
                           ' has_database_privilege(''%s'',datname,''CREATE WITH GRANT OPTION'') as CREATE_GRANT,'+
                           ' has_database_privilege(''%s'',datname,''TEMP WITH GRANT OPTION'') as TEMP_GRANT'+
                           ' FROM pg_database                                               ';

  sqlShowSchemaPrivelegesWithGrantOptsEx =  'SELECT nspname,                                '+
                           ' has_schema_privilege(''%s'',n.oid,''CREATE'') as CREATE,  '+
                           ' has_schema_privilege(''%s'',n.oid,''USAGE'') as USE,    '+
                           ' has_schema_privilege(''%s'',n.oid,''CREATE WITH GRANT OPTION'') as CREATE_GRANT,'+
                           ' has_schema_privilege(''%s'',n.oid,''USAGE WITH GRANT OPTION'') as USE_GRANT'+
                           ' FROM pg_namespace AS n                                         '+
                           ' WHERE True %NoSys                                              ';

  sqlShowTablespacePrivelegesWithGrantOptsEx  = 'SELECT spcname,                             '+
                           ' has_tablespace_privilege(''%s'',p.oid,''CREATE'') as CREATE,                                           '+
                           ' has_tablespace_privilege(''%s'',p.oid,''CREATE WITH GRANT OPTION'') as CREATE_GRANT'+
                           ' FROM pg_tablespace AS p                                        ';

  sqlShowFunctionsPrivilegesWithGrantOptsEx  = 'SELECT p.proname, n.nspname,                  '+
                           ' has_function_privilege(''%s'',p.oid,''EXECUTE'') as EXEC,                                          '+
                           ' has_function_privilege(''%s'',p.oid,''EXECUTE WITH GRANT OPTION'') as EXEC_GRANT'+
                           ' FROM pg_proc AS p, pg_namespace AS n                           '+
                           ' WHERE p.pronamespace = n.oid %NoSys                            ';

  sqlShowLanguagesPrivilegesWithGrantOptsEx  = 'SELECT l.lanname,                            '+
                           ' has_language_privilege(''%s'',l.oid,''USAGE'') as USE,                                            '+
                           ' has_language_privilege(''%s'',l.oid,''USAGE WITH GRANT OPTION'') as USE_GRANT'+
                           ' FROM pg_language AS l                                          '+
                           ' WHERE True                                                     ';

implementation

end.
