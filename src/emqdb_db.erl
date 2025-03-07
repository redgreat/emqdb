%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%%% @doc
%%%
%%% 数据库入库模块
%%%
%%% @end
%%% Created : 2025-3-1 17:08:55
%%%-------------------------------------------------------------------
-module(emqdb_db).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([db_pg_gnss/2, db_pg_lbs/2, db_pg_780eg/8, db_ora_sql/1, db_ora_map/1, db_ora_gnss/2]).

%%====================================================================
%% API 函数
%%====================================================================
%% @doc
%% 合宙gnss设备定位信息入库
%% @end
db_pg_gnss(Imei, {Lng, Lat}) ->
    try
        emqdb_pgpool:equery(pool_pg, "insert into lc_hzgnss(imei, lng, lat)
        values($1, $2, $3);", [Imei, Lng, Lat])
    catch
        Exception:Error -> 
            lager:error("Database Insert Failed: ~p:~p", [Exception, Error])
    end.

%% @doc
%% 合宙gnss设备定位信息入库
%% @end
db_pg_lbs(Imei, {Lng, Lat}) ->
    try
        emqdb_pgpool:equery(pool_pg, "insert into lc_hzgnss(imei, lng, lat, loctype)
        values($1, $2, $3, 1);", [Imei, Lng, Lat])
    catch
        Exception:Error -> 
            lager:error("Database Insert Failed: ~p:~p", [Exception, Error])
    end.
%% @doc
%% 合宙gnss设备定位信息入oracle库
%% @end
db_ora_gnss(Imei, {Lng, Lat}) ->
    try
        db_ora_sql({sql_query_to_str("insert into lc_hzgnss(imei, lng, lat) values(:1, :2, :3)"),
            sql_params_to_str([Imei, Lng, Lat])})
    catch
        Exception:Error -> 
            lager:error("Database Insert Failed: ~p:~p", [Exception, Error])
    end.

%% @doc
%% 合宙780eg定位信息入库
%% @end
db_pg_780eg(Imei, Lng, Lat, Height, Direction, Speed, Satellite, InsertTime) ->
    try
        emqdb_pgpool:equery(pool_pg, "insert into lc_hzgnss(imei, lng, lat, height, direction, speed, satellite, inserttime)
        values($1, $2, $3, $4, $5, $6, $7, $8);", [Imei, Lng, Lat, Height, Direction, Speed, Satellite, InsertTime])
    catch
        Exception:Error -> 
            lager:error("Database Insert Failed: ~p:~p", [Exception, Error])
    end.

%% @doc
%% 执行oracle 查询语句
%% @end
db_ora_map(Sql) ->
    convert_to_map(gen_server:call(emqdb_oraconn, {execute_sql, Sql})).

%% @doc
%% 执行oracle 插入语句
%% @end
db_ora_sql(Sql) ->
    gen_server:call(emqdb_oraconn, {execute_sql, Sql}).

%%====================================================================
%% 内部函数
%%====================================================================
convert_to_map({ok, [{result_set, Columns, _, Rows}]}) ->
    lists:map(fun(Row) -> row_to_map(Columns, Row) end, Rows).

row_to_map(Columns, Row) ->
    lists:foldl(fun({Column, Value}, Acc) ->
                    maps:put(binary_to_atom(Column, utf8), Value, Acc)
                end, #{}, lists:zip(Columns, Row)).

%% @private
%% @doc
%% 转换为字符串
%% @end
sql_query_to_str(SqlQuery) ->
    emqdb_conv:str(SqlQuery).

sql_params_to_str(Params) when is_list(Params) ->
    lists:map(
        fun
            (false) -> "0";
            (true) -> "1";
            (null) -> null;
            (Value) -> emqdb_conv:str(Value)
        end,
        Params
    ).