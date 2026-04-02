JLinkExe ip tunnel:69669681::jlink-europe.segger.com -Device GD32F330K8 -If SWD -Speed 1000 -CommandFile /dev/stdin << 'EOF'
r
h
loadfile dist/firmware/di4-firmware-gd32f330k8u6-4.9.hex
r
g
q
EOF