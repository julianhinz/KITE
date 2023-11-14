pacman::p_load(hexSticker)
sticker(

  # image
  "man/figures/kite.png",
  s_x=1.2, # slightly to right to appear centered
  s_y=1.2,
  s_width=0.725,
  s_height=1.4,

  # package name
  package="KITE",
  p_size=9.5,
  p_color = "#383838", # 00030A 010101
  p_x = 0.7,
  p_y = 0.7,

  # Output file
  filename="man/figures/logo_kite.png",

  # Background colour
  h_fill = "#FFFFFF", # #F0F0F0

  # Border
  # Grey colours: https://www.w3schools.com/colors/colors_shades.asp
  h_color = "#d35a3a",   # 3F4243 7F2B94 3B2691 4238AF
  # h_size = 1.5,

  dpi = 1000 # otherwise the final fantasy image quality is not good
)
