# pmon

A system/process monitor for Linux and Darwin based systems.

## Commands

Multiple calls can be made at the same time by seperating them with spaces.

Command    | Descroption
-----------|------------
$pid$      | orders processes by PID
$cpu$      | orders processes by CPU-usage
$mem$      | orders processes by memory-usage
$time$     | orders processes by uptime
$com$      | orders processes by command-name
$dec$      | sets the order direction to descending
$asc$      | sets the order direction to ascending
$p[pid]$   | pins/unpins process
$d[s]$     | sets the update delay (seconds)
$s[pid]$   | toggles graph-view of the specified process
$r$        | resets the settings to default
$q$        | quits the application

## Usage

To run the project, run:
```sh
cabal run
```
