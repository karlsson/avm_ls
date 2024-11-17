import Config
config :avm_ls,
  strip_type: :ws2812,
  strip_len: 1,
  di_pin: 8

config :logger,
  level: :info

## On board RGB LED GPIO (di_pin) on ESP32-XX-DevKiTs
## gpio_pin(esp32_c3) -> 8;
## gpio_pin(esp32_c6) -> 8;
## gpio_pin(esp32_h2) -> 8;
## gpio_pin(esp32_s2) -> 18;
## gpio_pin(esp32_s3) -> 38.
