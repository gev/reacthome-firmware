JLinkExe ip tunnel:69669681::jlink-europe.segger.com -Device GD32F330K8 -If SWD -Speed 1000 -CommandFile /dev/stdin << 'EOF'
erase
r
g
q
EOF