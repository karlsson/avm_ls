# avm_ls

## AtomVM LED strip walk

AtomVM implementation in Erlang and Elixir of "walking lights" on a Neopixel LED strip using the AtomVM SPI interface.
Some Gleam code is also compiled but only takes part in monitor printout for the moment.

## What it is

It is a simple Erlang implementation, using the gen_server behaviour to light LEDs on a Neopixel LED strip.
The LEDs are addressed with an index and set using RGB (or HSV) tuples. The server takes messages from
independent processes, multiplexes them and updates the LED strip on regular (200 ms) basis.
It has been tested on a ESP32-C3 SoC with a 60 leds ws2812 strip.

The protocol encoding is done in a configurable callback module. Skeletons for other strip types, sk9822 and ap102, have been added but not tested.

The main program in the AvmLs module runs for about 1 minute and spawns processes that each updates their own walking led independendtly. Walking speed and colour will differ between processes.

## Prerequisites
- You will need [Erlang OTP 27](https://www.erlang.org/downloads) or later installed in order to compile and build the documents properly.
- [Elixir](https://elixir-lang.org/install.html) 1.17 or later.
- [Gleam](https://gleam.run/getting-started/installing/).

All three languages are supported with [asdf](https://github.com/asdf-vm/asdf-erlang) plugins and can be installed using it.
- [MixGleam](https://github.com/gleam-lang/mix_gleam) plugin. Make sure to read the installation instruction on the github page.
- [AtomVM](https://www.atomvm.net/), [ExAtomVM](https://github.com/atomvm/ExAtomVM) Mix plugin and [Espressif SDK](https://www.atomvm.net/doc/main/getting-started-guide.html) (if you are using ESP32 SoCs). See AtomVM docs for proper installation.
- cd <platform_root>
- idf.py flash
- ./build/flash.sh -l ../../../build/libs/esp32boot/esp32boot.avm

The <platform_root> is set to <atomvm_root>/src/platforms/{esp32, stm32, etc}.

## Installation
- checkout avm_ls from github.
- mix deps.get
- mix deps.compile
- MIX_ENV=prod mix atomvm.packbeam  ## If not "prod" ExDoc beam files will get squashed into the flash image. 
- cd <platform_root>
- ./build/flash.sh <avm_ls_root>/avm_ls.avm
- idf.py monitor ## optional if restart wanted

## Running

- The program will reset and start after flashing
- Alternatively you can (re)start it with `idf.py monitor` command from <platform_root>

## Documentation

With OTP 27 ExDoc is used and `mix docs` will generate documentation for both Elixir and Erlang modules in one place. For Gleam this is not (yet?) the case. `gleam docs build` will generate docs for Gleam modules but into the build directory and not in the docs dito.

## Comments
I tried to build modules using Erlang, Elixir and Gleam into the the flash image just as a test.
This works, but for the Gleam case the documentation will end up in a different place from Erlang/Elixir.

Also if I include a package (like gleam_stdlib) from gleam, all compiled modules will be put into the flash image even though I might use only one (like gleam/io).
