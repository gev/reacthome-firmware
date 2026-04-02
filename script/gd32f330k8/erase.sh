JLinkExe -Device GD32F330K8 -If SWD -Speed 1000 -CommandFile /dev/stdin << 'EOF'
erase
r
g
q
EOF