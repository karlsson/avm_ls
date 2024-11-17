-module(avm_ls_sk9822).
-behaviour(avm_ls_strip).
-export([spi_config/1,
         build_stream/1]).

spi_config(Config) ->
    avm_ls_ap102:spi_config(Config).

build_stream(RGBIList) ->
    OrderFun = fun({R, G, B}) -> {B, G, R} end,
    avm_ls_ap102:build_stream(RGBIList, OrderFun).
