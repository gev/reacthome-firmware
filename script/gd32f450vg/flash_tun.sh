JLinkExe ip tunnel:69669681::jlink-europe.segger.com -Device GD32F450VG -If SWD -Speed 1000 -CommandFile /dev/stdin << 'EOF'
r
h
loadfile dist/firmware/rs_hub4-firmware-gd32f450vgt6-6.2.hex
r
g
q
EOF