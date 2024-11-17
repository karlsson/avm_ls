defmodule AvmLs do
  @moduledoc """
  `AvmLs` - Atom VM LED Strip walk.

  Walking leds with random colour and speed. Tested with ESP32-C3 SoC.
  """

  @doc """
  Start the application, callback for AtomVM init.
  Calls a loop that spawns one process for each led to walk the strip.
  Lasts for 6000 seconds.
  """
  @spec start() :: :ok

  def start() do
    strip_len = 60
    start_args = %{di_pin: 8, strip_type: :ws2812, strip_len: strip_len}
    {_, _pid} = :avm_ls_server.start_link(start_args)
    receive do after 500 -> :ok end
    :io.format(~c"Hello from Erlang!~n")
    :gleam@io.println("Hello from Gleam!")
    :gleam@io.debug(:avm_ls_hello.hello())
    light_led(1, 2000, {10,20,30})
    ## :timer.sleep(4000)
    ## strip_len = 39
    ## duration = 500
    ## Process.spawn(fn -> walking_led(strip_len, duration, {10,10,0}) end,[])
    loop(strip_len, 30)
    :timer.sleep(50000)
    :io.format(~c"Goodbye World!~n")
  end

  defp loop(_strip_len, 0) do
    :ok
  end
  defp loop(strip_len, n) do
    rand = :avm_ls_server.random()
    << duration::8, r::8, g::8, b::8 >> = << rand :: 32 >>
    duration = duration + 200
    b = div(b, 2)
    if (r + g) > 512 do
      Process.spawn(fn -> walking_led(strip_len, duration, {r, g, b}) end,[])
    else
      Process.spawn(fn -> walking_led_up(strip_len, duration, {r,g,b}) end, [])
    end
    receive do after 3000 -> :ok end
    loop(strip_len, n-1)
  end

  defp light_led(index, duration, {r,g,b}) do
    :avm_ls_server.set_led(index,{:rgb,{r,g,b}})
    :timer.sleep(duration)
    :avm_ls_server.clear_led(index)
  end

  defp walking_led(0, _, _) do
    :ok
  end
  defp walking_led(n, duration, {_r,_g,_b} = c) do
    light_led(n, duration, c)
    walking_led(n-1, duration, c)
  end

  defp walking_led_up(n, duration, {r,g,b}) do
    walking_led_up(n+1, 1, duration, {r,g,b})
  end

  defp walking_led_up(stop, stop, _, _) do
    :ok
  end
  defp walking_led_up(stop, n, duration, {_r,_g,_b} = c) do
    light_led(n, duration, c)
    walking_led_up(stop, n+1, duration, c)
  end
end
