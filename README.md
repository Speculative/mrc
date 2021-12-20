# mrc
## Running
```
stack build
stack exec mrc-exe +RTS -N8
```
## Profiling simulator performance
```
stack build --profile
stack exec mrc-exe +RTS -N8 -ls -p
```
## Plots
```
stack exec mrc-exe +RTS -N8 > out
python plot_mrc out
```