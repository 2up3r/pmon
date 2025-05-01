# pmon

A TUI process monitor for Linux and Darwin based systems.

## Usage

This project depends on `ps-info`.
To install `ps-info` clone it from [GitHub](https://github.com/2up3r/ps-info), into the folder where `pmon` is located:

```sh
$ git clone https://github.com/2up3r/ps-info
```

Then follow the instuctions to set up `ps-info`.
After `ps-info` is set up, you are ready to continue here.

### Running with cabal

To run the project through cabal move inside `pmon` and run:

```sh
$ cabal run
```

### Running with executable

Or to install the executable run (inside `pmon`):

```sh
$ cabal install
```

And now you can run `pmon` from anywhere using:

```sh
$ pmon
```

## Commands

Multiple calls can be made at the same time by seperating them with spaces.

Command  | Descroption
---------|------------
$pid$    | orders processes by PID
$cpu$    | orders processes by CPU-usage
$mem$    | orders processes by memory-usage
$time$   | orders processes by uptime
$com$    | orders processes by command-name
$dec$    | sets the order direction to descending
$asc$    | sets the order direction to ascending
$p[pid]$ | pins/unpins process
$d[s]$   | sets the update delay (seconds)
$s[pid]$ | toggles graph-view of the specified process
$r$      | resets the settings to default
$q/esc$  | quits the application
