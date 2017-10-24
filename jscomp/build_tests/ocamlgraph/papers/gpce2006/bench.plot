set style data line 

set terminal latex
set size 0.7,0.7
set key left

set xlabel "number of nodes $V$"
set output "bench1c.tex"
plot "ia_c" t "IA", "ic_c" t "IC", "pa_c" t "PA", "pc_c" t "PC"

set output "bench1d.tex"
plot "ia_d" t "IA", "ic_d" t "IC", "pa_d" t "PA", "pc_d" t "PC"

# plot "ia_mem_full" t "IA", "pa_mem_full" t "PA", "ic_mem_full" t "IC", "pc_mem_full" using ($1*$1):$2 t "PC"

set xlabel "maze width $N$"
set output "bench2dij.tex"
plot "ia_dij" t "IA", "ic_dij" t "IC", "pa_dij" t "PA", "pc_dij" t "PC"

set output "bench2dfs.tex"
plot "ia_dfs" t "IA", "ic_dfs" t "IC", "pa_dfs" t "PA", "pc_dfs" t "PC"

set output "bench2bfs.tex"
plot "ia_bfs" t "IA", "ic_bfs" t "IC", "pa_bfs" t "PA", "pc_bfs" t "PC"

set output "bench2mem.tex"
plot "ia_mem" t "IA", "ic_mem" t "IC", "pa_mem" t "PA", "pc_mem" t "PC"

