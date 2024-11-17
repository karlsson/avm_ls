-module(avm_ls_ap102).
-behaviour(avm_ls_strip).

-export([spi_config/1,
         build_stream/1,
         build_stream/2]).

-define(SPI_START_FRAME, 16#00000000).
-define(SPI_END_FRAME, 16#FFFFFFFF).
-define(ILUM_START_BITS, 2#111).
-define(SPI_CLOCK_HZ, 40000000).


spi_config(#{di_pin := DiPin, ci_pin := CiPin, name := DeviceName}) ->
    [
     {bus_config,
      [
       {miso, -1},       %% Not used
       {mosi, DiPin},
       {sclk, CiPin}
      ]},
     {device_config,
      [
       {DeviceName,
        [
         {cs, -1},
         {address_len_bits, 0},
         {command_len_bits, 0},
         {clock_speed_hz, ?SPI_CLOCK_HZ},
         {mode, 3}
        ]}
      ]}
    ].

build_stream(RGBIList) ->
    build_stream(RGBIList, fun({R, G, B}) -> {B, G, R} end).

build_stream(RGBIList, OrderFun) ->
    build_stream(RGBIList, OrderFun, []).

build_stream([{Red, Green, Blue, Illumination}| T], OrderFun, Acc) ->
    %% fast and dirty way to convert 0-100 Illumination into 5-bit Brightness.
    Bright = (Illumination * 31) div 97,
    {C1, C2, C3} = OrderFun({Red, Green, Blue}),
    LedN = << ?ILUM_START_BITS:3, Bright:5, C1:8, C2:8, C3:8 >>,
    build_stream(T, [LedN|Acc]);
build_stream([], _, Acc) ->
    list_to_binary([<<?SPI_START_FRAME:32>>, lists:reverse(Acc), <<?SPI_END_FRAME:32>>]).
