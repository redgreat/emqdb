%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%%% @doc
%%%
%%% 数据格式转换模块
%%% 源自：%%% https://github.com/emqx/emqx/blob/82e322246481ca61698c2c1ad37a21dc516fbf9b/apps/emqx_utils/src/emqx_utils_conv.erl
%%% @end
%%% Created : 2025-3-1 17:08:55
%%%-------------------------------------------------------------------
-module(emqdb_conv).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([bin/1]).
-export([str/1]).
-export([bool/1]).
-export([int/1]).
-export([float/1]).

-compile({no_auto_import, [float/1]}).

-type scalar() :: binary() | number() | atom() | string().

%%====================================================================
%% API 函数
%%====================================================================
%% @doc
%% 合宙gnss设备定位信息入库
%% @end
-spec bin(Term) -> binary() when
    Term :: scalar() | #{scalar() => Term} | [Term].
bin(Bin) when is_binary(Bin) -> Bin;
bin(Num) when is_number(Num) -> number_to_binary(Num);
bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
bin(Map) when is_map(Map) -> json:encode(Map);
bin(List) when is_list(List) ->
    case io_lib:printable_list(List) of
        true -> unicode:characters_to_binary(List);
        false -> json:encode(List)
    end;
bin(Data) ->
    error({invalid_bin, Data}).

-spec str(Term) -> string() when
    Term :: scalar() | #{scalar() => Term} | [Term].
str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
str(Num) when is_number(Num) -> number_to_list(Num);
str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
str(Map) when is_map(Map) -> binary_to_list(json:encode(Map));
str(List) when is_list(List) ->
    case io_lib:printable_list(List) of
        true -> List;
        false -> binary_to_list(json:encode(List))
    end;
str(Data) ->
    error({invalid_str, Data}).

-spec number_to_binary(number()) -> binary().
number_to_binary(Int) when is_integer(Int) ->
    integer_to_binary(Int);
number_to_binary(Float) when is_float(Float) ->
    float_to_binary(Float, [{decimals, 10}, compact]).

-spec number_to_list(number()) -> string().
number_to_list(Int) when is_integer(Int) ->
    integer_to_list(Int);
number_to_list(Float) when is_float(Float) ->
    float_to_list(Float, [{decimals, 10}, compact]).

-spec bool(Term) -> boolean() when
    Term :: boolean() | binary() | 0..1.
bool(true) -> true;
bool(<<"true">>) -> true;
bool(N) when N == 1 -> true;
bool(false) -> false;
bool(<<"false">>) -> false;
bool(N) when N == 0 -> false;
bool(Data) -> error(badarg, [Data]).

-spec int(Term) -> integer() when
    Term :: binary() | string() | number() | boolean().
int(List) when is_list(List) ->
    try
        list_to_integer(List)
    catch
        error:badarg ->
            int(list_to_float(List))
    end;
int(Bin) when is_binary(Bin) ->
    try
        binary_to_integer(Bin)
    catch
        error:badarg ->
            int(binary_to_float(Bin))
    end;
int(Int) when is_integer(Int) ->
    Int;
int(Float) when is_float(Float) ->
    erlang:floor(Float);
int(true) ->
    1;
int(false) ->
    0;
int(Data) ->
    error(badarg, [Data]).

-spec float(Term) -> float() when
    Term :: binary() | string() | number().
float(List) when is_list(List) ->
    try
        list_to_float(List)
    catch
        error:badarg ->
            float(list_to_integer(List))
    end;
float(Bin) when is_binary(Bin) ->
    try
        binary_to_float(Bin)
    catch
        error:badarg ->
            float(binary_to_integer(Bin))
    end;
float(Num) when is_number(Num) ->
    erlang:float(Num);
float(Data) ->
    error(badarg, [Data]).

%%====================================================================
%% 内部函数
%%====================================================================
