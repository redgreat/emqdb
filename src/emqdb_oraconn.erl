%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%%% @doc
%%%
%%% Oracle数据库连接管理
%%%
%%% @end
%%% Created : 2025-3-5 11:38:41
%%%-------------------------------------------------------------------
-module(emqdb_oraconn).
-author("wangcw").

-behaviour(gen_server).

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API 函数
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc
%% 初始化连接池
%% @end
init([]) ->
    try
      {ok, OracleConfig} = application:get_env(jamdb_oracle, orclite),
      Host = proplists:get_value(host, OracleConfig, "localhost"),
      Port = proplists:get_value(port, OracleConfig, 1521),
      Username = proplists:get_value(username, OracleConfig, undefined),
      Password = proplists:get_value(password, OracleConfig, undefined),
      ServiceName = proplists:get_value(servicename, OracleConfig, undefined),
      AppName = proplists:get_value(appname, OracleConfig, "emqdb"),
      Autocommit = proplists:get_value(autocommit, OracleConfig, 1),
      PoolSize = proplists:get_value(poolsize, OracleConfig, 2),

      true = is_list(Username) andalso length(Username) > 0,
      true = is_list(Password) andalso length(Password) > 0,
      true = is_list(ServiceName) andalso length(ServiceName) > 0,

      ConnOpts = [
        {host, Host},
        {port, Port},
        {user, Username},
        {password, Password},
        {service_name, ServiceName},
        {app_name, AppName},
        {autocommit, Autocommit},
        {pool_size, PoolSize}
      ],

      {ok, ConnRef} = jamdb_oracle:start(ConnOpts),
      lager:info("~p Connected to ~s at ~s with user ~s: ~p~n", [?MODULE, ServiceName, Host, Username, ConnRef]),
      case jamdb_oracle:sql_query(ConnRef, "ALTER SESSION SET NLS_TIMESTAMP_TZ_FORMAT = 'YYYY-MM-DD HH24:MI:SS TZH:TZM'") of
        {ok, _} ->
          lager:debug("Session time format set successfully");
        {error, Reason} ->
          lager:error("Failed to set session time format: ~p", [Reason])
      end,
      {ok, ConnRef}
    catch
      error:{badmatch, {undefined, _}} ->
        lager:error("Oracle config missing in app env");
      error:function_clause ->
        lager:error("Missing required oracle connection parameters");
      _:Error ->
        lager:error("Oracle pool init failed: ~p", [Error])
    end.

handle_call({execute_sql, Sql}, _From, ConnRef) ->
    % jamdb_oracle:sql_query(ConnRef, "COMON;"),
    case jamdb_oracle:sql_query(ConnRef, Sql) of
        {ok, Result} ->
            % lager:debug("SQL executed successfully: ~p", [Result]),
            {reply, {ok, Result}, ConnRef};
        {error, Reason} ->
            case Reason of
              {proc_result, Code, MsgCodes} ->
                  Msg = unicode:characters_to_binary(MsgCodes),
                  lager:error("ORA-~p: ~s", [Code, Msg]);
              _ ->
                  lager:error("SQL execution failed: ~p", [Reason])
    end,
            {reply, {error, Reason}, ConnRef}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, ConnRef) ->
    ok = jamdb_oracle:stop(ConnRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
