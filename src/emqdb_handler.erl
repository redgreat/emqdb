%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%%% @doc
%%%
%%% EMQX消息处理逻辑
%%%
%%% @end
%%% Created : 2025-02-14 15:04
%%%-------------------------------------------------------------------
-module(emqdb_handler).
-author("wangcw").

-behaviour(gen_server).

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% 资源
%%%===================================================================
% -include_lib("emqtt/include/emqtt.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server 函数
%%%===================================================================
init([]) ->
  {ok, Host} = application:get_env(emqdb, emqx_host),
  {ok, Port} = application:get_env(emqdb, emqx_port),
  {ok, Username} = application:get_env(emqdb, emqx_username),
  {ok, Password} = application:get_env(emqdb, emqx_password),
  {ok, ClientId} = application:get_env(emqdb, emqx_client_id),
  {ok, Topics} = application:get_env(emqdb, emqx_topics),

  Options = [
    {host, Host},
    {port, Port},
    {clientid, ClientId},
    {username, Username},
    {password, Password},
    {keepalive, 60},
    {clean_start, true}
  ],

  {ok, ClientPid} = emqtt:start_link(Options),
  lager:info("MQTT client process started: ~p~n", [ClientPid]),
    
  case emqtt:connect(ClientPid) of
    {ok, _} ->
      lager:info("Connected to EMQX broker at ~p:~p~n", [Host, Port]),
      lists:foreach(fun(Topic) ->
        case emqtt:subscribe(ClientPid, Topic, 0) of
          {ok, _, _} ->
            lager:info("Subscribed to MQTT topic: ~p~n", [Topic]);
          {error, Reason} ->
            lager:error("Failed to subscribe to topic ~p: ~p~n", [Topic, Reason])
        end
      end, Topics),
      {ok, #{mqtt_client => ClientPid}};
    {error, Reason} ->
      lager:error("Failed to connect to EMQX broker: ~p~n", [Reason]),
      {stop, {failed_to_connect, Reason}}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({publish, #{payload := Payload, topic := Topic}}, State) ->
  lager:info("Received MQTT message - Topic: ~p, Payload: ~p", [Topic, Payload]),
  case Topic of
    <<"pos/gnss/", Imei/binary>> ->
      lager:info("Processing GNSS data for IMEI: ~p", [Imei]),
      handle_gnss_data(Payload, Imei, State);
    <<"pos/780eg/", Imei/binary>> ->
      lager:info("Processing 780EG data for IMEI: ~p", [Imei]),
      handle_gnss_data(Payload, Imei, State);
    _ ->
      lager:warning("Received message on unknown topic: ~p", [Topic]),
      {noreply, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

handle_gnss_data(Payload, Imei, State) ->
  try
    lager:info("Raw payload: ~p", [Payload]),
    JsonData = json:decode(Payload),
    lager:info("Decoded JSON data: ~p", [JsonData]),
    
    TimeStamp = maps:get(<<"timestamp">>, JsonData),
    DateTime = calendar:system_time_to_universal_time(TimeStamp, second),
    Acc = maps:get(<<"acc">>, JsonData, 0),
    Csq = maps:get(<<"csq">>, JsonData, 0),
    Volt = maps:get(<<"volt">>, JsonData, 0),

    lager:info("Basic info - IMEI: ~p, Time: ~p, Acc: ~p, Csq: ~p, Volt: ~p", 
               [Imei, DateTime, Acc, Csq, Volt]),

    Gps = maps:get(<<"gps">>, JsonData, #{}),
    lager:info("GPS data: ~p", [Gps]),

    Spd = maps:get(<<"spd">>, Gps, 0),
    Alt = maps:get(<<"alt">>, Gps, 0),
    Dir = maps:get(<<"dir">>, Gps, 0),
    Sats = maps:get(<<"sats">>, Gps, 0),
    GpsLng = maps:get(<<"lng">>, Gps, undefined),
    GpsLat = maps:get(<<"lat">>, Gps, undefined),

    lager:info("GPS coordinates - Lng: ~p, Lat: ~p, Speed: ~p, Alt: ~p, Dir: ~p, Sats: ~p",
               [GpsLng, GpsLat, Spd, Alt, Dir, Sats]),

    Lbs = maps:get(<<"lbs">>, JsonData, #{}),
    lager:info("LBS data: ~p", [Lbs]),
    
    {LbsLng, LbsLat} = 
      case {maps:get(<<"lng">>, Lbs, undefined), maps:get(<<"lat">>, Lbs, undefined)} of
        {undefined, _} -> 
          lager:warning("Missing LBS longitude"),
          {undefined, undefined};
        {_, undefined} -> 
          lager:warning("Missing LBS latitude"),
          {undefined, undefined};
        {Lng, Lat} -> 
          {binary_to_float(Lng), binary_to_float(Lat)}
      end,

    lager:info("LBS coordinates - Lng: ~p, Lat: ~p", [LbsLng, LbsLat]),

    emqdb_db:db_pg_yed(DateTime, Imei, Acc, Csq, Volt, GpsLat, GpsLng, LbsLat, LbsLng, Alt, Dir, Spd, Sats),
    % emqdb_db:db_ora_yed(DateTime, Imei, Acc, Csq, Volt, GpsLat, GpsLng, LbsLat, LbsLng, Alt, Dir, Spd, Sats),

    {noreply, State}
  catch
    _:Error:Stacktrace ->
      lager:error("Failed to parse message: ~p~nPayload: ~p~nStack trace: ~p", 
                 [Error, Payload, Stacktrace]),
      {noreply, State}
  end.

terminate(_Reason, State) ->
  lager:info("Handler terminating."),
  case maps:get(mqtt_client, State, undefined) of
    Pid when is_pid(Pid) -> emqtt:stop(Pid);
    _ -> ok
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% 内部函数
%%%===================================================================
%% @doc
%% 接收经纬度数据
%% @end
% parse_gnss_message(<<>>) ->
%  {error, empty_message};
% parse_gnss_message(Binary) when is_binary(Binary) ->
%  case binary_to_list(Binary) of
%    [] -> {error, empty_message};
%    Str -> parse_gnss_string(Str)
%  end;
% parse_gnss_message(_) ->
%  {error, invalid_format}.

%% @doc
%% 解析经纬度字符串（格式："经度_纬度"）
%% @end
% parse_gnss_string(Str) ->
%  try
%    case string:split(Str, "_") of
%      [LngStr, LatStr] ->
%        Lng = list_to_float(LngStr),
%        Lat = list_to_float(LatStr),
%        % emqdb_geo:wgs84_to_gcj02({Lng, Lat});
%        {Lng, Lat};
%      _ ->
%        lager:info("Payload Data Error!")
%    end
%  catch
%    error:_ -> {error, invalid_coordinate_format}
%  end.


%% @doc
%% 时间格式转换（时间戳 --> 年-月-日：时:分:秒）
%% @end
timestamp_to_binary(Timestamp) when is_integer(Timestamp) ->
  DateTime = calendar:system_time_to_universal_time(Timestamp, second),
  datetime_to_binary(DateTime).

datetime_to_binary({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  list_to_binary(lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                            [Year, Month, Day, Hour, Minute, Second]))).