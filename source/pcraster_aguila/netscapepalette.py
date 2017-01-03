# Netscape Navigator's color palette on Windows
# Netscape Navigator uses a 216-color palette (a 6x6x6 color cube) on
# Microsoft Windows 3.1, 3.11, 95, 98, and NT when your system is running
# in 8-bit color mode (256 colors on screen). The value for each element
# of the color -- red, green, and blue -- will always be one of these:
#      0,  51, 102, 153, 204, 255 (decimal)
# 
#          00,  33,  66,  99,  CC,  FF (hexadecimal)
# 
# Therefore, if you only use the following color values in the images
# on your web pages, they will display cleanly for people who are using
# Netscape Navigator on Windows to view your pages. If you use any colors
# that are not in the following list, then Netscape Navigator on Windows
# will dither them, sometimes with undesirable results.

print("static const size_t netscapePaletteSize = 216;")
print("static const UINT2 netscapePaletteMaxVal = 255;")
print("static UINT2 netscapePalette[][3] = \n{")

for red in range(0, 256, 51):
  for green in range(0, 256, 51):
    for blue in range(0, 256, 51):
      print("  { %d, %d, %d }," % (red, green, blue))

print("};")
