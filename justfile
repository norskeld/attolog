default: run

# Run Attolog with the default REPL.
@run:
  dotnet run --project Attolog

# Run Attolog with rlwrap for a slightly better experience in REPL.
@rl:
  rlwrap -a -N -t dumb dotnet run --project Attolog
