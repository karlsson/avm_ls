%
%
% Copyright 2024 Mikael Karlsson <mikael.karlsson@creado.se>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%
% Many Espressif ESP32 Devkit boards have an RGB LED of type "LED strip"
% See also: https://components.espressif.com/components/espressif/led_strip

-module(avm_ls_server).
-moduledoc """
The LED Strip gen_server module.

""".
-behaviour(gen_server).

%% API
-export([start_link/1, set_led/2, clear_led/1, random/0]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-define(MOD_PRE_FIX, "avm_ls_").
-define(SERVER, ?MODULE).
-type col() :: 0..255.
-type ill() :: 0..100.

-doc "Different ways of setting the LED color and illumination".
-type colours() :: {rgb, {col(), col(), col()}} |
                   {rgbi, {col(), col(), col(), ill()}} |
                   {hsv, {0..360, 0..100, ill()}}.

-type strip_type() :: avm_ls_strip:strip_type().
-type strip_len() :: non_neg_integer().

-type start_args() :: #{
                        strip_len := strip_len(),
                        strip_type := strip_type(),
                        di_pin := non_neg_integer(),
                        ci_pin => non_neg_integer()
                        }.

-type state() :: #{ led_array := map(),
                    spi := pid(),
                    strip_len := strip_len(),
                    strip_index := non_neg_integer(),
                    device_name := strip_type(),
                    cbm := module()
                  }.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-doc "Starts the server".
-spec start_link(Args :: start_args()) -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).


-doc "Set the spedific LED colour. Index starts at 0".
-spec set_led(Index::non_neg_integer(), Colours::colours()) ->
          ok | {error, index_too_large}| no_return().
set_led(Index, {rgb, {_R, _G, _B}} = C)      -> set_led1(Index, C);
set_led(Index, {rgbi, {_R, _G, _B, _I}} = C) -> set_led1(Index, C);
set_led(Index, {hsv, {_H, _S, _V}} = C)      -> set_led1(Index, C).

set_led1(Index, C) ->
    gen_server:cast(?SERVER, {set_led, {Index, C}, self()}).

-doc "Clear the spedific LED.".
-spec clear_led(Index::non_neg_integer()) -> ok | {error, index_too_large} | no_return().
clear_led(Index) ->
    gen_server:cast(?SERVER, {clear_led, Index, self()}).

random() -> atomvm:random().

-spec init(Args::start_args()) -> state().
init(Args) ->
    io:format("Args2 ~p~n",[Args]),
    process_flag(trap_exit, true),
    StripLen = maps:get(strip_len, Args, 1),
    StripType = maps:get(strip_type, Args, ws2812),
    StripTypeStr = atom_to_binary(StripType, utf8),
    CallBackMod = binary_to_atom(<< ?MOD_PRE_FIX, StripTypeStr/binary >>, utf8),
    SPIConfigPars = #{
                      di_pin => maps:get(di_pin, Args),
                      ci_pin => maps:get(ci_pin, Args, -1),
                      name => StripType
                     },
    SPIConfig = CallBackMod:spi_config(SPIConfigPars),
    SPI = spi:open(SPIConfig),
    %% ARR = maps:from_keys(lists:seq(1, StripLen), #{self() => {rgbi,{0,0,0,0}}}),
    ARR = maps:from_keys(lists:seq(1, StripLen), #{}),
    State =
        #{led_array => ARR,
          spi => SPI,
          strip_len => StripLen,
          strip_index => StripLen,
          cbm => CallBackMod,
          device_name => StripType
         },
    erlang:send_after(2000, self(), update_led_strip),
    {ok, State}.


%%% gen_server callbacks
-spec handle_call(Request :: term(), From :: gen_server:from(),
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()}.
handle_call({set_led, {Index, Colours}}, {Pid, _Tag},
            #{led_array := Arr, strip_len := Len, strip_index := SI} = State) ->
    link(Pid),
    if
        Index =< Len ->
            NewArr = update_array(Index, Arr, Pid, Colours),
            {reply, ok,
             State#{led_array := NewArr, strip_index := max(Index, SI)}};
        true ->
            {reply, {error, index_too_large}, State}
    end;
handle_call({clear_led, Index}, {Pid, _Tag},
            #{led_array := Arr, strip_len := Len, strip_index := SI} = State) ->
    if
        Index =< Len ->
            NewArr = remove_pid_from_array(Index, Pid, Arr),
            {reply, ok,
             State#{led_array := NewArr, strip_index := max(Index, SI)}};
        true ->
            {reply, {error, index_too_large}, State}
    end;
handle_call(_, _, _) ->
  error(not_implemented).

-spec handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
handle_cast({set_led, {Index, Colours}, Pid},
            #{led_array := Arr, strip_len := Len, strip_index := SI} = State) ->
    link(Pid),
    if
        Index =< Len ->
            NewArr = update_array(Index, Arr, Pid, Colours),
            {noreply,
             State#{led_array := NewArr, strip_index := max(Index, SI)}};
        true ->
            {noreply, State}
    end;
handle_cast({clear_led, Index, Pid},
            #{led_array := Arr, strip_len := Len, strip_index := SI} = State) ->
    if
        Index =< Len ->
            NewArr = remove_pid_from_array(Index, Pid, Arr),
            {noreply,
             State#{led_array := NewArr, strip_index := max(Index, SI)}};
        true ->
            {noreply, State}
    end;
handle_cast(_, _) ->
  error(not_implemented).

-spec handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.

handle_info({'EXIT', Pid, _Reason}, #{led_array := Arr, strip_index := SI} = State) ->
    {LastIndex, NewArr} =
        maps:fold(
          fun(Index, M, {I, ArrAcc}) ->
                  case is_map_key(Pid, M) of
                      true ->
                          {max(Index,I), maps:put(Index, maps:remove(Pid, M), ArrAcc)};
                      false ->
                          {I, ArrAcc}
                  end
          end, {0, Arr}, Arr),
    {noreply, State#{led_array := NewArr, strip_index := max(LastIndex, SI)}};
handle_info(update_led_strip, State) ->
    NewState = update_led_strip(State),
    erlang:send_after(200, self(), update_led_strip),
    {noreply, NewState}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
terminate(_Reason, #{spi := SPI}) ->
    spi:close(SPI).


%% Private functions

update_array(Index, Array, Pid, Value) ->
    Map = maps:get(Index, Array),
    NewMap = Map#{Pid => Value},
    maps:put(Index, NewMap, Array).

remove_pid_from_array(Index, Pid, Array) ->
    Map = maps:get(Index, Array),
    NewMap = maps:remove(Pid, Map),
    maps:put(Index, NewMap, Array).

update_led_strip(#{led_array := Arr, cbm := CBM, spi := SPI, strip_index := SI, device_name := Name}
                 = State) when SI > 0 ->
    M1 = maps:filter(fun(Key, _Value) -> (Key =< SI) andalso (Key > 0) end,  Arr),

    F = fun(_Index, M) -> maps:fold(fun(_Pid, V, Acc) ->
                                    sum_rgbi(V, Acc)
                            end, {0,0,0,0}, M) end,
    M2 = maps:map(F, M1),
    M3 = maps:map(fun(_Index,{R,B,G,I}) ->
                           {min(R, 255), min(B, 255), min(G, 255), min(I, 100)}
                   end, M2),
    %% io:format("M3 ~p~n", [M3]),
    L3 = [ V || {_, V} <- lists:sort(maps:to_list(M3))], 
    WriteData = CBM:build_stream(L3),
    ok = spi:write(SPI, Name, #{write_data => WriteData}),
    State#{strip_index := 0};
update_led_strip(State) ->
    State.

sum_rgbi({rgbi, {R, B, G, I}}, {R0, B0, G0, I0}) ->
    {R + R0, B + B0, G + G0, I + I0};
sum_rgbi({rgb, {R, B, G}}, {R0, B0, G0, I0}) ->
    {R + R0, B + B0, G + G0, I0};
sum_rgbi({hsv, {H, S, V}}, {R0, B0, G0, I0}) ->
    {R,G,B} = write_dot_hsv({H, S, V}),
    {R + R0, B + B0, G + G0, V + I0}.


write_dot_hsv({H, S, V}) ->
    RGB_max = (V * 255) div 100,
    RGB_min = (RGB_max * (100 - S)) div 100,
    Diff = H rem 60,
    RGB_adj = ((RGB_max - RGB_min) * Diff) div 60,
    Sextant = H div 60,
    case Sextant of
        0 -> {RGB_max, RGB_min + RGB_adj, RGB_min};
        1 -> {RGB_max - RGB_adj, RGB_max, RGB_min};
        2 -> {RGB_min, RGB_max, RGB_min + RGB_adj};
        3 -> {RGB_min, RGB_max - RGB_adj, RGB_max};
        4 -> {RGB_min + RGB_adj, RGB_min, RGB_max};
        _ -> {RGB_max, RGB_min, RGB_max - RGB_adj}
    end.
