/// -moduledoc """
///   `avm_ls_hello` - Hello from Gleam.
///
///   Simple Gleam module that returns the thing World.
/// """.
import gleam/io

pub type Thing {
  World
}

// -doc """
//   Start the application, callback for AtomVM init.
//   Calls a loop that spawns one process for each led to walk the strip.
//   Lasts for 6000 seconds.
//   """.
pub fn hello() -> Thing {
  World
}

pub fn hoho(string: String) -> Nil {
  io.println("Ho ho " <> string)
}
