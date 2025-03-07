-- Active: 1740732360630@@fn.wongcw.cn@1521@ORCLPDB@WANGCW
SELECT * 
FROM lc_hzgnss
ORDER BY inserttime DESC
LIMIT 10;

SELECT COUNT(1)
FROM lc_hzgnss
LIMIT 10;

insert into lc_hzgnss(topic, lng, lat)
values('pos/gns', 120.5211292, 36.411615);

truncate table lc_hzgnss;

SELECT pg_get_serial_sequence('lc_hzgnss', 'id');

ALTER SEQUENCE public.lc_hzgnss_id_seq RESTART WITH 1;

select max(id) from public.lc_hzgnss;


-------------------------------------------------------------------------------
------------------------------------------------------------------------------- ORACLE
-------------------------------------------------------------------------------
SELECT column_name, data_type, data_length, data_precision, data_scale, nullable
FROM user_tab_columns
WHERE table_name = 'lc_hzgnss';

SELECT *
FROM lc_hzgnss
ORDER BY inserttime DESC
FETCH FIRST 100 ROWS ONLY;

SELECT *
FROM lc_hzgnss
WHERE ROWNUM <= 100
ORDER BY inserttime DESC;


SELECT * FROM "WANGCW"."LC_HZGNSS" ORDER BY InsertTime DESC;


INSERT INTO LC_HZGNSS("imei", "lat", "lng", "height", "direction", "speed", "satellite", "inserttime")
VALUES ('pos/gnss', 36.4105400, 120.4941000, 80, 90, 25, 32, '2025-03-03 13:30:49'),
('pos/gnss', 36.4105400, 120.4941000, 80, 90, 25, 32, '2025-03-03 13:30:49');

INSERT INTO LC_HZGNSS("imei", "lat", "lng", "height", "direction", "speed", "satellite", "inserttime")
VALUES ('pos/gnss', 36.4105400, 120.4941000, 80, 90, 25, 32, TO_DATE('2025-03-03 13:30:49', 'YYYY-MM-DD HH24:MI:SS'));

INSERT INTO LC_HZGNSS("imei", "lat", "lng", "height", "direction", "speed", "satellite", "inserttime")
VALUES ('pos/gnss', 36.4105400, 120.4941000, 80, 90, 25, 32, TO_DATE('2025-03-03 13:30:49', 'YYYY-MM-DD HH24:MI:SS'));

SELECT * FROM dual;

INSERT INTO LC_HZGNSS("imei", "lat", "lng", "height", "direction", "speed", "satellite", "inserttime")
VALUES ('pos/gnss', 36.4105400, 120.4941000, 80, 90, 25, 32, TO_DATE('2025-03-03 13:30:49', 'YYYY-MM-DD HH24:MI:SS'));

INSERT ALL
INTO "WANGCW"."LC_HZGNSS" ("IMEI", "LAT", "LNG", "HEIGHT", "DIRECTION", "SPEED", "SATELLITE", "INSERTTIME")
VALUES ('123456789012345', 36.4105400, 120.4941000, 80, 90, 25, 32, TO_TIMESTAMP_TZ('2025-03-03 13:30:49 +08:00', 'YYYY-MM-DD HH24:MI:SS TZH:TZM'))
INTO "WANGCW"."LC_HZGNSS" ("IMEI", "LAT", "LNG", "HEIGHT", "DIRECTION", "SPEED", "SATELLITE", "INSERTTIME")
VALUES ('987654321098765', 37.1234567, 121.6543210, 100, 180, 30, 28, TO_TIMESTAMP_TZ('2025-03-03 14:45:20 +08:00', 'YYYY-MM-DD HH24:MI:SS TZH:TZM'));

SELECT * FROM "WANGCW"."LC_HZGNSS";

-- 插入第1条数据
INSERT INTO "WANGCW"."LC_HZGNSS" ("IMEI", "LAT", "LNG", "HEIGHT", "DIRECTION", "SPEED", "SATELLITE", "INSERTTIME")
VALUES ('123456789012345', 36.4105400, 120.4941000, 80, 90, 25, 32, TO_TIMESTAMP_TZ('2025-03-03 13:30:49 +08:00', 'YYYY-MM-DD HH24:MI:SS TZH:TZM'));

-- 插入第2条数据
INSERT INTO "WANGCW"."LC_HZGNSS" ("IMEI", "LAT", "LNG", "HEIGHT", "DIRECTION", "SPEED", "SATELLITE", "INSERTTIME")
VALUES ('987654321098765', 37.1234567, 121.6543210, 100, 180, 30, 28, TO_TIMESTAMP_TZ('2025-03-03 14:45:20 +08:00', 'YYYY-MM-DD HH24:MI:SS TZH:TZM'));

-- 插入第3条数据
INSERT INTO "WANGCW"."LC_HZGNSS" ("IMEI", "LAT", "LNG", "HEIGHT", "DIRECTION", "SPEED", "SATELLITE", "INSERTTIME")
VALUES ('556677889900112', 38.2345678, 122.7654321, 120, 270, 35, 30, TO_TIMESTAMP_TZ('2025-03-03 15:50:30 +08:00', 'YYYY-MM-DD HH24:MI:SS TZH:TZM'));

-- 插入第4条数据
INSERT INTO "WANGCW"."LC_HZGNSS" ("IMEI", "LAT", "LNG", "HEIGHT", "DIRECTION", "SPEED", "SATELLITE", "INSERTTIME")
VALUES ('112233445566778', 39.3456789, 123.8765432, 150, 45, 40, 33, TO_TIMESTAMP_TZ('2025-03-03 16:15:45 +08:00', 'YYYY-MM-DD HH24:MI:SS TZH:TZM'));

-- 插入第5条数据
INSERT INTO "WANGCW"."LC_HZGNSS" ("IMEI", "LAT", "LNG", "HEIGHT", "DIRECTION", "SPEED", "SATELLITE", "INSERTTIME")
VALUES ('998877665544332', 40.4567890, 124.9876543, 180, 135, 45, 35, TO_TIMESTAMP_TZ('2025-03-03 17:20:50 +08:00', 'YYYY-MM-DD HH24:MI:SS TZH:TZM'));

select * from "WANGCW"."LC_HZGNSS";

SELECT * FROM "WANGCW"."LC_HZGNSS" ORDER BY InsertTime DESC;

-- 执行失败的原因:
-- 1. 字符串引号没有闭合,缺少右侧单引号
-- 2. 数值不需要加引号
-- 3. 字段名建议加双引号
-- 正确写法如下:

INSERT INTO "lc_hzgnss"("imei", "lng", "lat") 
VALUES ('123456789012345', 36.4105400, 120.4941000);

INSERT INTO lc_hzgnss(imei, lng, lat)
VALUES ('123456789012345', 36.4105400, 120.4941000);

 insert into lc_hzgnss("IMEI", "LAT", "LNG")
 values("123456789012345", 36.4105400, 120.4941000);

 
insert into lc_hzgnss(imei, lng, lat)
        values($1, $2, $3);

insert into lc_hzgnss(imei, lng, lat)
        values('860678076874157', 36.41071, 120.4938);

select * from lc_hzgnss order by inserttime desc;

<<"860678076874157">>, {120.4938,36.41073}

emqdb_pgpool:equery("insert into lc_hzgnss(imei, lng, lat)
        values($1, $2, $3);", [<<"860678076874157">>, 120.4938, 36.41073]).

emqdb_db:db_ora_gnss("860678076874157", {120.4938, 36.41073}).

emqdb_db:db_ora_ins({"insert into lc_hzgnss(imei, lng, lat)
        values(:1, :2, :3)", [<<"860678076874157">>, 120.4938, 36.41073]}).

emqdb_pgpool:equery("insert into lc_hzgnss(imei, lng, lat) values($1, $2, $3);", [<<"860678076874157">>, 120.4938, 36.41073]).

emqpg_pgpool:equery("insert into lc_hzgnss(imei, lng, lat) values($1, $2, $3);", [<<"860678076874157">>, 120.4938, 36.41073]).