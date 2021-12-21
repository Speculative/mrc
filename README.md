# mrc
## CLI
```
mrc-exe <execution strategy> <workloads> <cache sizes>
execution strategy: serial | parallel
workloads: uniform,skewed,arc
cache sizes: 3,4,5,6,7,8...
```
- No spaces between comma separated values
- Larger cache sizes take longer to execute
- Arc traces take a while to execute

## Plots
```
stack exec -- mrc-exe parallel uniform,skewed 3,4,5,6,7,8 +RTS -N8
# copy output to a file
# Output redirection doesn't work due to encoding issues
python plot_mrc outFile
```
## Profiling simulator performance
```
stack build --profile
stack exec -- mrc-exe parallel uniform,skewed 3,4,5,6,7,8 +RTS -N8 -ls -p
```