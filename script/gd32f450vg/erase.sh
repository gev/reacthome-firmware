JLinkExe -Device GD32F450VG -If SWD -Speed 1000 -CommandFile /dev/stdin << 'EOF'
erase
r
g
q
EOF