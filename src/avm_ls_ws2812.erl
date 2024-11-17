-module(avm_ls_ws2812).
-moduledoc """
Neopixel (ws2812) callback module.
Implements `avm_ls_strip` behaviour.
""". 

-behaviour(avm_ls_strip).
-export([spi_config/1,
         build_stream/1]).

-doc "Only mosi is used for ws2812. Clock speed set to 2.4 MHz".
-spec spi_config(avm_ls_strip:spi_conf()) -> list().
spi_config(#{di_pin := DiPin, name := DeviceName}) when is_integer(DiPin) ->
    [
     {bus_config,
      [
       {miso, -1},       %% Not used
       {mosi, DiPin},
       {sclk, -1}        %% Not used
      ]},
     {device_config,
      [
       {DeviceName,
        [
         {clock_speed_hz, 2400000}, %% 3 SPI bits per LED bit ~ 1.25 uS
         {mode, 0},
         {cs, -1}, %% Not used
         {address_len_bits, 8}
        ]}
      ]}
    ].

-spec build_stream(RGBIList::avm_ls_strip:rgbi_list()) -> binary().
build_stream(RGBIList) ->
    build_stream(RGBIList, fun({R, G, B}) -> {G, R, B} end).

build_stream(RGBList, OrderFun) ->
    build_stream(RGBList, OrderFun, []).
build_stream([{R, G, B, _I}|T], OrderFun, Acc) ->
    LedN = led_strip_bytes(OrderFun({R, G, B})),
    build_stream(T, OrderFun, [LedN|Acc]);
build_stream([], _, Acc) ->
    list_to_binary(lists:reverse(Acc)).

%% 1 RGB LED will be 72 SPI bits <=> 9 bytes
led_strip_bytes({C1, C2, C3}) ->
    CB1 = led_strip_bits(C1),
    CB2 = led_strip_bits(C2),
    CB3 = led_strip_bits(C3),
    << CB1:24, CB2:24, CB3:24 >>. % Order is GRB in the stream.

%% WS2812 LED SPI 1 bit in will be 3 bits out
%% Bit = 1 -> 110, 0.8 us high, 0.45 us low at 2.4 MHz
%% Bit = 0 -> 100. 0.4 us high, 0.85 us low
%% One byte will be 24 SPI bits ( 3 bytes )
led_strip_bits(A) when A >= 0, A < 256, is_integer(A) ->
    led_strip_bits(A, 7, 0).

led_strip_bits(_A, N, Acc) when N < 0 -> Acc;
led_strip_bits(A, N, Acc) ->
    SPIbits =
        case (A bsr N) band 16#01 of
            1 -> 6; %% 110
            0 -> 4  %% 100
        end,
    led_strip_bits(A, N-1, (Acc bsl 3) bor SPIbits).
