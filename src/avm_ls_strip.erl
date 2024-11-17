-module(avm_ls_strip).
-moduledoc """
Behaviour for implementing SPI controlled LED strip types
in callback modules. Callback modules are named `avm_ls_<strip_type>`.
""".

-type strip_type() :: ws2812 | ap102 | sk9822 | atom().
-type rgbi() :: {avm_ls_server:col(), avm_ls_server:col(), avm_ls_server:col(),
                 avm_ls_server:ill()}.
-type rgbi_list() :: [rgbi()].

-type spi_conf() :: #{
                      di_pin => integer(), %% GPIO pin for SPI mosi (optional)
                      ci_pin => integer(), %% GPIO pin for SPI sclk (ap102, sk9822)
                      name := strip_type() %% Device name
                     }.

-callback spi_config(Pars) -> list() when
      Pars::spi_conf().

-callback build_stream(rgbi_list()) -> binary().
