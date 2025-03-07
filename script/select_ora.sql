-- Active: 1740732360630@@fn.wongcw.cn@1521@ORCLPDB@WANGCW
ALTER SESSION SET TIME_ZONE = '+08:00';

ALTER SYSTEM SET TIME_ZONE = '+08:00';

-- 设置日期格式包含时区
ALTER SESSION SET NLS_TIMESTAMP_TZ_FORMAT = 'YYYY-MM-DD HH24:MI:SS TZH:TZM';

SELECT * FROM LC_HZGNSS ORDER BY INSERTTIME DESC;

emqdb_db:db_ora_sql("COMMIT").

emqdb_db:db_ora_gnss(<<"860678076874157">>,{120.5211266,36.4116166}).

ConnOpts = [
        {host, "fn.wongcw.cn"},
        {port, 1521},
        {user, "wangcw"},
        {password, "Mm19890425"},
        {service_name, "orclpdb"},
        {app_name, "emqdb"},
        {autocommit, 1}
      ].

{ok, ConnRef} = jamdb_oracle:start(ConnOpts).

jamdb_oracle:sql_query(ConnRef, "COMON;"),

jamdb_oracle:sql_query(ConnRef, {"insert into ",<<"lc_hzgnss">>, "(", <<"imei">>, <<"lng">>, <<"lat">>, ")"
        values(:1, :2, :3)", [<<"860678076874158">>, 120.5211268, 36.4116168]}).

jamdb_oracle:sql_query(ConnRef, "COMMIT").

SELECT * FROM LC_HZGNSS WHERE IMEI='860678076874158' ORDER BY INSERTTIME DESC;

insert into lc_hzgnss(imei, lng, lat)
        values('860678076874158', 120.5211268, 36.4116168);

ROLLBACK;