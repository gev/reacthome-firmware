JLinkExe ip tunnel:69651308::jlink-europe.segger.com -Device GD32F330K8 -If SWD -Speed 1000 -CommandFile /dev/stdin << 'EOF'
r
h
loadfile dist/firmware/di4-3-4_10-0_1-gd32f330k8u6.hex
r
g
q
EOF